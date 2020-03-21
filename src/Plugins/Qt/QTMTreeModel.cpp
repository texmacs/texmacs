
/******************************************************************************
 * MODULE     : QTMTreeModel.hpp
 * DESCRIPTION: A QAbstractItemModel based on TeXmacs trees.
 * COPYRIGHT  : (C) 2013 Miguel de Benito Delgado
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "QTMGuiHelper.hpp"
#include "QTMTreeModel.hpp"

#include "path.hpp"
#include "observer.hpp"
#include "modification.hpp"

#include "qt_utilities.hpp"

#include <QPixmap>

const char* QTMTreeModel::bad_tree_data_exception =
  "QTMTreeModel: couldn't parse provided data";
const char* QTMTreeModel::no_observer_exception =
  "QTMTreeModel: tree lacks tree pointer or inverse path observer";

/*! Creates a QTMTreeModel attached to a given tree or returns the one already
    attached if present.
 
 See the documentation for QTMTreeModel for the format for data roles.
 
 @note The tree pointer is marked const because we don't retain the tree_rep
       inside. However, we do attach observers to the tree, so maybe it 
       shouldn't be const after all.
 */
QTMTreeModel*
QTMTreeModel::instance (const tree& data, const tree& roles, QObject* parent) {
  tree& t = const_cast<tree&> (data);
  if (!is_nil (data) && !is_nil (t->obs) &&
      t->obs->get_type () == OBSERVER_WIDGET)
    return static_cast<qt_tree_observer_rep*> (t->obs.rep)->model;
  else
    return new QTMTreeModel (data, roles, parent);
}

/*! Internal construction of QTMTreeModels
 
 Construction of QTMTreeModels is done using QTMTreeModel::instance () to ensure
 only one model is built and attached to a given tree. This is done internally
 with a qt_tree_observer attached to trees. This observer takes ownership of
 the QTMTreeModel.
 */
QTMTreeModel::QTMTreeModel (const tree& data, const tree& roles, QObject* p)
: QAbstractItemModel (p), _t_rep (data.rep) {
  tree t = tree (_t_rep);
  parse_roles (roles);
  const path& ip = obtain_ip (t);
  if (is_nil (ip) || const_cast<path&>(ip)->item == DETACHED)
    attach_ip (t, path(0));

    // the qt_tree_observer takes ownership of this object
  attach_observer (t, qt_tree_observer (this));
}

QTMTreeModel::~QTMTreeModel () {
    // We must not detach our observers here: the qt_tree_observer_rep is the
    // owner of this object: if we are here, it is dying
}

inline int
QTMTreeModel::row_offset (const tree& t) const {
  return _roles[L(const_cast<tree&>(t))][NumberOfArguments];
}

tree
QTMTreeModel::item_from_index (const QModelIndex& index) const {
  if (!index.isValid()) return tree (_t_rep);
  return tree (static_cast<tree_rep*> (index.internalPointer()));
}

inline QModelIndex
QTMTreeModel::index_from_item (const tree& tref) const {
  tree& t = const_cast<tree&> (tref);
  tree _t = tree (_t_rep);
  path ip = obtain_ip (t);
  if (ipath_has_parent (ip)) {
    path   p = reverse (ip->next) / reverse (obtain_ip (_t));// Look one item up
    if (is_nil (p)) return createIndex (ip->item, 0, t.rep); // parent is root?
    tree& pt = subtree (_t, p);                              // pt is the parent
    int  row = ip->item - row_offset (pt);
    return createIndex (row, 0, t.rep);
  }
  return QModelIndex();
}

bool
QTMTreeModel::hasIndex (int row, int column, const QModelIndex& parent) const {
  (void) column;
  const tree& t = item_from_index (parent);
  return is_compound(t) && row + row_offset(t) < N(t) && column == 0;
}

QModelIndex
QTMTreeModel::index (int row, int column, const QModelIndex& parent) const {
  if (!hasIndex (row, column, parent)) return QModelIndex();
  tree t = item_from_index (parent);

  if (is_compound (t) && row + row_offset(t) < N(t))
    return createIndex (row, column, t[row + row_offset(t)].rep);
  else
    return QModelIndex();
}

QModelIndex
QTMTreeModel::parent (const QModelIndex& index) const {
  if (!index.isValid()) return QModelIndex();
  tree  t = item_from_index (index);
  path ip = obtain_ip (t);
  if (ipath_has_parent (ip)) {
    path p = reverse (ip->next);            // Look one item up in the path
    if (is_nil (p)) return QModelIndex();
    tree  _t = tree (_t_rep);
    p        = p / reverse (obtain_ip(_t));
    tree& pt = subtree (_t, p);             // pt is the parent
    return index_from_item (pt);
  }
  return QModelIndex();
}

int
QTMTreeModel::rowCount (const QModelIndex& parent) const {
  tree t = item_from_index (parent);
  return is_compound (t) ? N(t) - row_offset (t) : 0;
}

int
QTMTreeModel::columnCount (const QModelIndex& parent) const {
  (void) parent;
  return 1;
}

  /// FIXME: this method is called quite often, maybe some (more?) caching is
  // necessary
QVariant
QTMTreeModel::data (const QModelIndex& index, int role) const {
  const tree& tref = item_from_index (index);
  tree& t = const_cast<tree&> (tref);
  int pos = _roles.contains(L(t)) && _roles[L(t)].contains(role)
              ? _roles[L(t)][role] : -1;
  if (role >= TMUserRole && pos > -1 && !is_atomic (t) && N(t) >= pos && 
      is_atomic (t[pos]))
    return QVariant (to_qstring (t[pos]->label));
  
  switch (role) {
    case Qt::DisplayRole:
    case Qt::EditRole:
    case Qt::ToolTipRole:
    case Qt::StatusTipRole:
      if (is_atomic (t))
        return to_qstring (t->label);
      else if (pos == -1 || !is_atomic (t[pos]))
        return to_qstring (as_string (L(t)));
      else
        return to_qstring (t[pos]->label);
    case Qt::DecorationRole:
    {
        // NOTE: By calling QPixmap's constructor with a file name, I'm bypassing
        // TeXmacs' caching of pictures in qt_load_xpm(), but since I need a
        // QPixmap and TeXmacs stores QImages this is preferable because Qt uses
        // QPixmapCache for all files it loads.
        // It might actually be better to cache the QIcons first and return them
      url u = "$TEXMACS_PIXMAP_PATH";
      if (pos == -1 || is_atomic (t) || !is_atomic (t[pos]))
        u = resolve ("$TEXMACS_PIXMAP_PATH" *
                     url ("treelabel-" * as_string (L(t)) * ".xpm"));
      else
        u = resolve (url (t[pos]->label));
      if (is_rooted_name (u))
        return QPixmap (to_qstring (concretize (u)));
      else
        return QVariant();
    }
      break;
    case CommandRole:
      if (pos > -1 && !is_atomic (t) && N(t) >= pos && is_atomic (t[pos]))
        return QVariant (to_qstring (t[pos]->label));
      else
        return QVariant();
    case Qt::SizeHintRole:
    default:
      return QVariant();
  }
  return QVariant();
}

bool
QTMTreeModel::hasChildren (const QModelIndex& parent) const {
  tree t = item_from_index (parent);
  return is_compound (t) && N(t) > row_offset(t);
}

Qt::ItemFlags
QTMTreeModel::flags (const QModelIndex& index) const {
  (void) index;
    // FIXME: to do (plus draggable, etc.)
  return Qt::ItemIsSelectable | Qt::ItemIsEnabled;
}

QVariant
QTMTreeModel::headerData (int section, Qt::Orientation orient, int role) const {
  (void) section; (void) orient; (void) role;
  return QVariant();
}

inline bool
QTMTreeModel::ipath_has_parent (const path& ip) const {
    // Careful: our root _t might be a subtree of some other tree in TeXmacs
    // already with inverse paths which lead further up the hierarchy
    // than we manage. That's why we need the extra check with obtain_ip()
  tree _t = tree (_t_rep);
  return !is_nil (ip) && const_cast<path&>(ip)->item != DETACHED
                      && obtain_ip (_t) != ip;
}

void
QTMTreeModel::parse_roles (const tree& roles) {
  tree& r = const_cast<tree&> (roles);
  
  if (!is_compound (roles)) return;
  
  for (int i = 0; i < N(roles); ++i) {
    if (is_compound (r[i])) {
      tree_label tag = L(r[i]);
      _roles[tag][NumberOfArguments] = N(r[i]);
      for (int j = 0; j < N(r[i]); ++j) {
        ASSERT (is_atomic (r[i][j]), "QTMTreeModel: bad format declaration");
        string role = r[i][j]->label;
        if      (role == "DisplayRole")    _roles[tag][Qt::DisplayRole]    = j;
        else if (role == "EditRole")       _roles[tag][Qt::EditRole]       = j;
        else if (role == "DecorationRole") _roles[tag][Qt::DecorationRole] = j;
        else if (role == "ToolTipRole")    _roles[tag][Qt::ToolTipRole]    = j;
        else if (role == "CommandRole")    _roles[tag][CommandRole]        = j;
        else if (string ("UserRole:") <= role) {
          int num = max (0, min (9, as_int (role (9, N(role))) - 1));          
          _roles[tag][TMUserRole + num] = j;
        }
      }
    }
  }
}


/******************************************************************************
 * qt_tree_observer_rep
 ******************************************************************************/

qt_tree_observer_rep::~qt_tree_observer_rep () {
  delete model;
}

void
qt_tree_observer_rep::announce (tree& ref, modification mod) {
  (void) ref; (void) mod;
  //if (mod->k != MOD_SET_CURSOR)
  //  model->beginResetModel();
  /* MOD_SET_CURSOR is announced only for TeXmacs buffers. We might want to also
   keep a pointer to a QItemSelectionModel, or maybe some QTMItemSelectionModel
   (which would allow for intermediate proxy models used by the views, as does
   KProxyItemSelectionModel (see 
   http://steveire.wordpress.com/2010/04/20/sharing-a-qitemselection-between-views-through-proxy-models/ ))
   
   In this case we could tell the views that they might want to update their selections, e.g.
   
     selection->setCurrentIndex (model->index_from_item (mod->t));
   
   Or if we have a QTMTreeSelectionModel:
   
     selection_model->set_cursor (ref, mod);
   
   and QTMTreeSelectionModel::set_cursor would do something like:
   
     if (current_item != mod->t) {
       current_item = mod->t;
       emit currentChanged (index_from_item (current_item));
     }
   
   The problem is we may not want all views to see their selections follow the
   cursor position. Furthermore, this only makes sense for buffers...
    */
}

void
qt_tree_observer_rep::done (tree& ref, modification mod) {
  (void) ref; (void) mod;
  //if (mod->k != MOD_SET_CURSOR)
  //  model->endResetModel();
}

observer
qt_tree_observer (QTMTreeModel* model) {
  return tm_new<qt_tree_observer_rep> (model);
}

