
/******************************************************************************
 * MODULE     : QTMTreeModel.hpp
 * DESCRIPTION: A QAbstractItemModel based on TeXmacs trees.
 * COPYRIGHT  : (C) 2013 Miguel de Benito Delgado
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef QTMTREEMODEL_HPP
#define QTMTREEMODEL_HPP

#include <QAbstractItemModel>

#include "tree.hpp"
#include "hashmap.hpp"
#include "observer.hpp"

class QTMTreeModel;

/*! An observer to keep a tree in sync with a QTMTreeModel (QAbstractItemModel).
 
 This object does two things:
  
  * it tells the model when to notify its views of changes.
  * it owns the QTMTreeModel and deletes it when the observer is detached from
    the tree and deleted.
 
 FIXME: Currently we just beginResetModel() in announce() and endResetModel()
        in done(). This is wasteful and will scale very badly.
 */
class qt_tree_observer_rep : public observer_rep {
  QTMTreeModel* model;

public:
  explicit qt_tree_observer_rep (QTMTreeModel* _model)
  : observer_rep(), model (_model) { }
  virtual ~qt_tree_observer_rep ();
  
  virtual int  get_type () { return OBSERVER_WIDGET; }
  
  virtual void announce (tree& ref, modification mod);
  virtual void     done (tree& ref, modification mod);

  friend class QTMTreeModel;
};

observer qt_tree_observer (QTMTreeModel* model);

/*! QTMTreeModel is a QT model around a TeXmacs tree.
 
 This object installs a qt_tree_observer at the root of the tree it is
 passed, which acts as a proxy for announcements and notifications of
 modifications performed on the tree elements by e.g. the editor. Currently,
 we are very crude and simply reset the whole (!) model each time there's a
 modification. This is extremely wasteful, but a complete implementation
 requires much more work, because there are tree operations which don't
 translate easily into the beginInsertRows / endInsertRows framework of
 QAbstractItemModel. In particular, raw_insert_node replaces a node and this
 isn't either an insertion or a deletion. In order to keep the views in sync
 we could:
 
  * Keep a local copy of the whole tree and decompose TeXmacs' operations
    into Qt's, keeping things in sync.
  * Tag subtrees as "dirty" when they are about to be edited and return phony
    ones when the views request updates
  * ...
 
 Anyway: it's too much work for now.
 */

class QTMTreeModel : public QAbstractItemModel {
  Q_OBJECT
  
  static const char* bad_tree_data_exception;
  static const char*   no_observer_exception;
  
  static const int DETACHED = -5;
  
  tree_rep*         _t_rep;  //!< Our data. Must be tree_rep or we have a cycle!
  path             _prefix;  //!< Path of the root _t.
  path            _iprefix;  //!< Inverse path of the root _t (for convenience).
  
  hashmap<string, QVariant> roles;
  
  QTMTreeModel (const tree& data, QObject* parent = 0);
  QTMTreeModel (const QTMTreeModel& _other);
  QTMTreeModel& operator= (QTMTreeModel& other);
  
public:
  virtual ~QTMTreeModel ();
  static QTMTreeModel* instance (const tree& data, QObject* parent = 0);
  
  virtual QModelIndex   index (int row, int column,
                              const QModelIndex& parent = QModelIndex()) const;
  virtual bool       hasIndex (int row, int column,
                               const QModelIndex& parent) const;
  virtual QModelIndex  parent (const QModelIndex& index) const;
  virtual int        rowCount (const QModelIndex& parent = QModelIndex()) const;
  virtual int     columnCount (const QModelIndex& parent = QModelIndex()) const;
  virtual QVariant       data (const QModelIndex& index,
                               int role = Qt::DisplayRole) const;
  virtual Qt::ItemFlags flags (const QModelIndex& index) const;
  virtual QVariant headerData (int section, Qt::Orientation orientation,
                               int role = Qt::DisplayRole) const;
  virtual bool    hasChildren (const QModelIndex& parent = QModelIndex()) const;
  
  friend class qt_tree_observer_rep;
  
private:
  QModelIndex index_from_item (tree& ref) const;
  tree        item_from_index (const QModelIndex& index) const;
  bool       ipath_has_parent (const path& ip) const;
  void            parse_roles (const tree& roles);
};

#endif // QTMTREEMODEL_HPP
