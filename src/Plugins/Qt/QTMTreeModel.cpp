
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
 
 @note The tree pointer is marked const because we don't retain the tree_rep
       inside.
 */
QTMTreeModel*
QTMTreeModel::instance (const tree& data, QObject* parent) {
  tree& t = const_cast<tree&> (data);
  if (!is_nil (data) && !is_nil (t->obs) &&
      t->obs->get_type () == OBSERVER_WIDGET)
    return static_cast<qt_tree_observer_rep*> (t->obs.rep)->model;
  else
    return new QTMTreeModel (data, parent);
}

/*! Internal construction of QTMTreeModels
 
 Construction of QTMTreeModels is done using QTMTreeModel::instance () to ensure
 only one model is built and attached to a given tree. This is done internally
 with a qt_tree_observer attached to trees. This observer takes ownership of
 the QTMTreeModel.

 */
QTMTreeModel::QTMTreeModel (const tree& data, QObject* parent)
: QAbstractItemModel (parent), _t_rep (data.rep) {
  tree t = tree (_t_rep);
  if (ipath_has_parent (obtain_ip (t))) _iprefix = obtain_ip (t);
  else                                  attach_ip (t, _iprefix << 0);
  _prefix = reverse (_iprefix);

    // the qt_tree_observer takes ownership of this object
    // there
  attach_observer (t, qt_tree_observer (this));
}

QTMTreeModel::~QTMTreeModel () {
    /// Don't do (something like) this:
    //clean_all_observers (_t->obs, OBSERVER_IP);
    // If the tree is part of a document, TeXmacs won't particularly like it
    // that we delete all of its inverse paths. If it isn't, then it's most
    // likely about to be discarded anyway.
}

tree
QTMTreeModel::item_from_index (const QModelIndex& index) const {
  if (!index.isValid()) return tree (_t_rep);
  return tree (static_cast<tree_rep*> (index.internalPointer()));
}

inline QModelIndex
QTMTreeModel::index_from_item (tree& ref) const {
  return createIndex (0, 0, ref.rep);  // FIXME: is this ok?
}

bool
QTMTreeModel::hasIndex (int row, int column, const QModelIndex& parent) const {
  (void) column;
  const tree& t = item_from_index (parent);
  return is_compound(t) && N(t) > row;
}

QModelIndex
QTMTreeModel::index (int row, int column, const QModelIndex& parent) const {
  if (!hasIndex (row, column, parent)) return QModelIndex();
  tree pt = item_from_index (parent);

  if (is_compound (pt) && row < N(pt))
    return createIndex (row, column, inside (pt[row]));
  else
    return QModelIndex();
}

QModelIndex
QTMTreeModel::parent (const QModelIndex& index) const {
  if (!index.isValid()) return QModelIndex();
  tree t = item_from_index (index);
  path ip = obtain_ip (t);
  if (ipath_has_parent (ip)) {
    path p = reverse (ip->next); // Look one item up in the path
    if (is_nil (p)) return QModelIndex();
    p = p / _prefix;
    tree _t = tree (_t_rep);
    tree& t = subtree (_t, p); // t is the parent
    int   i = is_nil (p) ? 0 : p->item;
    return createIndex (i, 0, t.rep);
  }
  return QModelIndex();
}

int
QTMTreeModel::rowCount (const QModelIndex& parent) const {
  tree t = item_from_index (parent);
  return is_compound (t) ? N(t) : 0;
}

int
QTMTreeModel::columnCount (const QModelIndex& parent) const {
  (void) parent;
  return 1;
}

QVariant
QTMTreeModel::data (const QModelIndex& index, int role) const {
  tree t = item_from_index (index);
  QString qs;
  if (is_atomic (t)) qs = to_qstring (t->label);
  else               qs = to_qstring (as_string (L(t)));

  /* TODO */
  switch (role) {
      // The key data to be rendered in the form of text (QString)
    case Qt::DisplayRole:
      return qs;
       // The data to be rendered as a decoration (QColor, QIcon or QPixmap)
    case Qt::DecorationRole:
      return QPixmap (qs);
       // The data in a form suitable for editing in an editor (QString)
    case Qt::EditRole:
      return qs;
       // The data displayed in the item's tooltip (QString)
    case Qt::ToolTipRole:
      return qs;
       // The data displayed in the status bar (QString)
    case Qt::StatusTipRole:
      return qs;
       // The data displayed for the item in "What's This?" mode (QString)
    case Qt::WhatsThisRole:
      return qs;
        // The size hint for the item that will be supplied to views (QSize)
    case Qt::SizeHintRole:
    default:
      return QVariant();
  }
}

bool
QTMTreeModel::hasChildren (const QModelIndex& parent) const {
  tree t = item_from_index (parent);
  return is_compound (t) && N(t) > 0;  // redundant?
}

Qt::ItemFlags
QTMTreeModel::flags (const QModelIndex& index) const {
  (void) index;
    // FIXME: to do (plus draggable, etc.)
  return Qt::ItemIsSelectable | Qt::ItemIsEnabled;
}

QVariant
QTMTreeModel::headerData (int section, Qt::Orientation orientation,
                          int role) const {
  (void) section; (void) orientation; (void) role;
  return QVariant();
}

inline bool
QTMTreeModel::ipath_has_parent (const path& ip) const {
    // Careful: our root _t might be a subtree of some other tree in TeXmacs
    // already with inverse paths which lead further up the hierarchy
    // than we manage. That's why we need the extra check with _iprefix
  
    // ************ FIXME! What if the original tree is moved!!!
  
  return !is_nil (ip) && const_cast<path&>(ip)->item != DETACHED
                      && _iprefix != ip;
}

void
QTMTreeModel::parse_roles (const tree& roles) {
  if (!is_compound (roles))
    return;
    // TODO
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
  if (mod->k != MOD_SET_CURSOR)
    model->beginResetModel();
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
  if (mod->k != MOD_SET_CURSOR)
    model->endResetModel();
}

observer
qt_tree_observer (QTMTreeModel* model) {
  return tm_new<qt_tree_observer_rep> (model);
}

