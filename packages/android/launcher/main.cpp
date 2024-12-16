#include <pthread.h>
#include <QDebug>

#define STACK_SIZE 0x1000000

#include <QCoreApplication>
#include <QtCore/private/qandroidextras_p.h>

bool checkPermission() {
  QList<bool> permissions;

  auto r = QtAndroidPrivate::checkPermission("android.permission.READ_EXTERNAL_STORAGE").result();
  if (r != QtAndroidPrivate::Authorized)
  {
      r = QtAndroidPrivate::requestPermission("android.permission.READ_EXTERNAL_STORAGE").result();
      if (r == QtAndroidPrivate::Denied)
          permissions.append(false);
  }
  r = QtAndroidPrivate::checkPermission("android.permission.WRITE_EXTERNAL_STORAGE").result();
  if (r != QtAndroidPrivate::Authorized)
  {
      r = QtAndroidPrivate::requestPermission("android.permission.WRITE_EXTERNAL_STORAGE").result();
      if (r == QtAndroidPrivate::Denied)
          permissions.append(false);
  }
  r = QtAndroidPrivate::checkPermission("android.permission.READ_MEDIA_IMAGES").result();
  if (r != QtAndroidPrivate::Authorized)
  {
      r = QtAndroidPrivate::requestPermission("android.permission.READ_MEDIA_IMAGES").result();
      if (r == QtAndroidPrivate::Denied)
          permissions.append(false);
  }
  return (permissions.count() != 3);
}

void texmacs_init_guile_hooks();
int texmacs_entrypoint(int argc, char** argv);

typedef struct android_args {
  int argc;
  char **argv;
} android_args;

void *main_thread (void* args) {
  android_args *a = (android_args *)args;
  int argc = a->argc;
  char **argv = a->argv;
  qDebug() << "Initializing Guile hooks...";
  texmacs_init_guile_hooks();
  qDebug() << "Checking permissions...";
  checkPermission();
  qDebug() << "Starting TeXmacs...";
  texmacs_entrypoint(argc, argv);
  return NULL;
}

int main(int argc, char *argv[])
{
    android_args args;
    args.argc = argc;
    args.argv = argv;

    qDebug() << "Initializing pthread attributes...";
    pthread_attr_t attr;
    int  rc;
    void* tmstack;
    size_t tmstacksize = STACK_SIZE;    
    if (pthread_attr_init (&attr) == -1) {
        qWarning() << "error in pthread_attr_init";
        return 1;
    }

    /* Get a big enough stack and align it on 4K boundary. */
    qDebug() << "Allocating stack storage...";
    tmstack = malloc (tmstacksize + PTHREAD_STACK_MIN);
    if (tmstack != NULL) {
        qDebug() << "Using PTHREAD_STACK_MIN to align stack address";
        tmstack = (void*)((((unsigned long long) tmstack + (PTHREAD_STACK_MIN - 1)) / PTHREAD_STACK_MIN) * PTHREAD_STACK_MIN);
    } else {
        qWarning() << "Error: unable to acquire stack storage";
        return 1;
    }

    qDebug() << "Setting stack address and size...";
    rc = pthread_attr_setstack (&attr, tmstack, tmstacksize);
    if (rc != 0) {
        qWarning() << "Error: pthread_attr_setstack returned " << QString::number(rc);
        return 1;
    }

    qDebug() << "Launching texmacs thread...";
    pthread_t tmthread;
    void *ret;
    if (pthread_create (&tmthread, &attr, main_thread, &args) != 0) {
        qWarning() << "Error: pthread_create failed";
        return 1;
    }
    if (pthread_join (tmthread, &ret) != 0) {
        qWarning() << "Error: pthread_join failed";
        return 1;
    }

    rc = pthread_attr_destroy (&attr);
    if (rc != 0) {
        qWarning() << "Error: pthread_attr_destroy returned " << QString::number(rc);
        return 1;
    }

    qDebug() << "TeXmacs thread finished";
    return 0;
}

