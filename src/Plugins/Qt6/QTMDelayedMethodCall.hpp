/******************************************************************************
* MODULE     : QTMDelayedMethodCall.hpp
* DESCRIPTION: Utility class to regroup the call of a method
* COPYRIGHT  : (C) 2026 Liza Belos
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef QTMDELAYEDMETHODCALL_HPP
#define QTMDELAYEDMETHODCALL_HPP

#if QT_VERSION >= 0x060000

#include "qt_utilities.hpp"

#include <QObject>
#include <QTimer>

#include <functional>
#include <type_traits>

template<typename T>
class QTMDelayedMethodInvoker : public QObject {
public:
  QTMDelayedMethodInvoker(T* instance, void (T::*method)(), QObject *parent = nullptr)
      : QObject(parent), mInstance(instance), mMethod(method) {}

  void onTimeout() {
    if (mInstance) (mInstance.data()->*mMethod)();
  }

private:
  QPointer<T> mInstance;
  void (T::*mMethod)();
};

class QTMDelayedMethodCall {

public:
  template<typename T>
  QTMDelayedMethodCall (T* instance, void (T::*method)(), int _delay_ms = 100) 
  : delay_ms(_delay_ms), is_first_call(true) {
    timer = new QTimer();
    timer->setSingleShot(true);
    auto *invoker = new QTMDelayedMethodInvoker<T>(instance, method, timer);
    QObject::connect(timer, &QTimer::timeout,
                     invoker, &QTMDelayedMethodInvoker<T>::onTimeout);
  }

  template<typename Functor>
  QTMDelayedMethodCall(Functor callback, int _delay_ms = 100) 
  : delay_ms(_delay_ms), is_first_call(true) {
      timer = new QTimer();
      timer->setSingleShot(true);
      QObject::connect(timer, &QTimer::timeout, std::move(callback));
  }

  inline ~QTMDelayedMethodCall () {
    timer->stop();
    delete timer;
  }

  QTMDelayedMethodCall (const QTMDelayedMethodCall&) = delete;
  QTMDelayedMethodCall& operator= (const QTMDelayedMethodCall&) = delete;
  QTMDelayedMethodCall (QTMDelayedMethodCall&&) = delete;
  QTMDelayedMethodCall& operator= (QTMDelayedMethodCall&&) = delete;

  inline void operator() () {
    if (is_first_call) {
      timer->start(0);
      is_first_call = false;
      return;
    }
    timer->stop();
    timer->start(delay_ms);
  }

  bool isActive() const {
    return timer->isActive();
  }

private:
    int delay_ms;
    bool is_first_call;
    QTimer* timer;
};

#define QTM_DECL_DELAYED(Method) \
    QTMDelayedMethodCall delayed##Method{ \
      std::bind(&std::remove_reference<decltype(*this)>::type::Method, this) }

#define QTM_CALL_DELAYED(Method) \
    delayed##Method()

#else

#define QTM_DECL_DELAYED(Method)
#define QTM_CALL_DELAYED(Method) Method()

#endif

#endif // QTMDELAYEDMETHODCALL_HPP