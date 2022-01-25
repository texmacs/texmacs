/****************************************************************************
** Meta object code from reading C++ file 'QTWKGuiHelper.hpp'
**
** Created by: The Qt Meta Object Compiler version 67 (Qt 5.15.2)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include <memory>
#include "QTWKGuiHelper.hpp"
#include <QtCore/qbytearray.h>
#include <QtCore/qmetatype.h>
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'QTWKGuiHelper.hpp' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 67
#error "This file was generated using the moc from 5.15.2. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
QT_WARNING_PUSH
QT_WARNING_DISABLE_DEPRECATED
struct qt_meta_stringdata_QTWKGuiHelper_t {
    QByteArrayData data[10];
    char stringdata0[106];
};
#define QT_MOC_LITERAL(idx, ofs, len) \
    Q_STATIC_BYTE_ARRAY_DATA_HEADER_INITIALIZER_WITH_OFFSET(len, \
    qptrdiff(offsetof(qt_meta_stringdata_QTWKGuiHelper_t, stringdata0) + ofs \
        - idx * sizeof(QByteArrayData)) \
    )
static const qt_meta_stringdata_QTWKGuiHelper_t qt_meta_stringdata_QTWKGuiHelper = {
    {
QT_MOC_LITERAL(0, 0, 13), // "QTWKGuiHelper"
QT_MOC_LITERAL(1, 14, 7), // "refresh"
QT_MOC_LITERAL(2, 22, 0), // ""
QT_MOC_LITERAL(3, 23, 13), // "tmSlotRefresh"
QT_MOC_LITERAL(4, 37, 6), // "string"
QT_MOC_LITERAL(5, 44, 8), // "doUpdate"
QT_MOC_LITERAL(6, 53, 9), // "doRefresh"
QT_MOC_LITERAL(7, 63, 19), // "doPopWaitingWidgets"
QT_MOC_LITERAL(8, 83, 17), // "emitTmSlotRefresh"
QT_MOC_LITERAL(9, 101, 4) // "kind"

    },
    "QTWKGuiHelper\0refresh\0\0tmSlotRefresh\0"
    "string\0doUpdate\0doRefresh\0doPopWaitingWidgets\0"
    "emitTmSlotRefresh\0kind"
};
#undef QT_MOC_LITERAL

static const uint qt_meta_data_QTWKGuiHelper[] = {

 // content:
       8,       // revision
       0,       // classname
       0,    0, // classinfo
       6,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       2,       // signalCount

 // signals: name, argc, parameters, tag, flags
       1,    0,   44,    2, 0x06 /* Public */,
       3,    1,   45,    2, 0x06 /* Public */,

 // slots: name, argc, parameters, tag, flags
       5,    0,   48,    2, 0x0a /* Public */,
       6,    0,   49,    2, 0x0a /* Public */,
       7,    0,   50,    2, 0x0a /* Public */,
       8,    1,   51,    2, 0x0a /* Public */,

 // signals: parameters
    QMetaType::Void,
    QMetaType::Void, 0x80000000 | 4,    2,

 // slots: parameters
    QMetaType::Void,
    QMetaType::Void,
    QMetaType::Void,
    QMetaType::Void, 0x80000000 | 4,    9,

       0        // eod
};

void QTWKGuiHelper::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    if (_c == QMetaObject::InvokeMetaMethod) {
        auto *_t = static_cast<QTWKGuiHelper *>(_o);
        Q_UNUSED(_t)
        switch (_id) {
        case 0: _t->refresh(); break;
        case 1: _t->tmSlotRefresh((*reinterpret_cast< string(*)>(_a[1]))); break;
        case 2: _t->doUpdate(); break;
        case 3: _t->doRefresh(); break;
        case 4: _t->doPopWaitingWidgets(); break;
        case 5: _t->emitTmSlotRefresh((*reinterpret_cast< string(*)>(_a[1]))); break;
        default: ;
        }
    } else if (_c == QMetaObject::IndexOfMethod) {
        int *result = reinterpret_cast<int *>(_a[0]);
        {
            using _t = void (QTWKGuiHelper::*)();
            if (*reinterpret_cast<_t *>(_a[1]) == static_cast<_t>(&QTWKGuiHelper::refresh)) {
                *result = 0;
                return;
            }
        }
        {
            using _t = void (QTWKGuiHelper::*)(string );
            if (*reinterpret_cast<_t *>(_a[1]) == static_cast<_t>(&QTWKGuiHelper::tmSlotRefresh)) {
                *result = 1;
                return;
            }
        }
    }
}

QT_INIT_METAOBJECT const QMetaObject QTWKGuiHelper::staticMetaObject = { {
    QMetaObject::SuperData::link<QObject::staticMetaObject>(),
    qt_meta_stringdata_QTWKGuiHelper.data,
    qt_meta_data_QTWKGuiHelper,
    qt_static_metacall,
    nullptr,
    nullptr
} };


const QMetaObject *QTWKGuiHelper::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->dynamicMetaObject() : &staticMetaObject;
}

void *QTWKGuiHelper::qt_metacast(const char *_clname)
{
    if (!_clname) return nullptr;
    if (!strcmp(_clname, qt_meta_stringdata_QTWKGuiHelper.stringdata0))
        return static_cast<void*>(this);
    return QObject::qt_metacast(_clname);
}

int QTWKGuiHelper::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        if (_id < 6)
            qt_static_metacall(this, _c, _id, _a);
        _id -= 6;
    } else if (_c == QMetaObject::RegisterMethodArgumentMetaType) {
        if (_id < 6)
            *reinterpret_cast<int*>(_a[0]) = -1;
        _id -= 6;
    }
    return _id;
}

// SIGNAL 0
void QTWKGuiHelper::refresh()
{
    QMetaObject::activate(this, &staticMetaObject, 0, nullptr);
}

// SIGNAL 1
void QTWKGuiHelper::tmSlotRefresh(string _t1)
{
    void *_a[] = { nullptr, const_cast<void*>(reinterpret_cast<const void*>(std::addressof(_t1))) };
    QMetaObject::activate(this, &staticMetaObject, 1, _a);
}
QT_WARNING_POP
QT_END_MOC_NAMESPACE
