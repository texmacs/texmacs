
/******************************************************************************
* MODULE     : android_system.cpp
* DESCRIPTION: Android system function proxies
* COPYRIGHT  : (C) 2024 Liza Belos
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "android_system.hpp"
#include "config.h"

#include <QApplication>
#include <QGuiApplication>
#include <QStyleHints>
#include <QFile>
#include <QString>
#include <QDir>
#include <QDirIterator>
#include <QIODevice>
#include <QTextStream>
#include <QDebug>
#include <QDateTime>

extern "C" {
#include "libguile/system.h"
#include "libguile/fports.h"
}

inline QString texmacs_string_to_qstring(string utf8_string) {
  return QString::fromUtf8(&utf8_string[0], N(utf8_string));
}

inline string texmacs_qstring_to_string(const QString &local_string) {
  return string(
    local_string.toUtf8().data(),
    (int)local_string.size()
  );
}

inline string texmacs_qbytearray_to_string(const QByteArray &local_string) {
  return string(
    local_string.data(),
    (int)local_string.size()
  );
}

void texmacs_lock_file(FILE *&file) {
  // do nothing
}

void texmacs_unlock_file(FILE *&file) {
  // do nothing
}

FILE* texmacs_fopen(string filename, string mode, bool lock) {
  QString qfilename = texmacs_string_to_qstring(filename);
  QFile *pfile = new QFile(qfilename);
  QFile::OpenMode open_mode = QFile::NotOpen;
  for (int i = 0; i < N(mode); i++) {
    switch (mode[i]) {
      case 'r':
        open_mode |= QFile::ReadOnly;
        break;
      case 'w':
        open_mode |= QFile::WriteOnly;
        break;
      case 'a':
        open_mode |= QFile::Append;
        break;
      case '+':
        open_mode |= QFile::ReadWrite;
        break;
      case 'b':
        // do nothing
        break;
      default:
        break;
    }
  }
  if (!pfile->open(open_mode)) {
    return nullptr;
  }
  return (FILE*)pfile;
}

ssize_t texmacs_fwrite (const char *s, size_t n, FILE *stream) {
  if (stream == stdout) {
    qDebug() << QString::fromUtf8(s, n);
    return n;
  }
  if (stream == stderr) {
    qWarning() << QString::fromUtf8(s, n);
    return n;
  }
  QFile *pfile = (QFile*)stream;
  int written = pfile->write(s, n);
  if (written == -1) {
    return 0;
  }
  return written;
}

ssize_t texmacs_fsize (FILE *stream) {
  QFile *pfile = (QFile*)stream;
  return pfile->size();
}

ssize_t texmacs_fread (char *z, size_t n, FILE *stream) {
  QFile *pfile = (QFile*)stream;
  return pfile->read(z, n);
}

void texmacs_fclose(FILE *&file, bool unlock) {
  QFile *pfile = (QFile*)file;
  pfile->close();
  delete pfile;
  file = nullptr;
}

typedef struct texmacs_android_dir_t {
  QDirIterator *iterator;
  QString dirname;
} texmacs_android_dir_t;

TEXMACS_DIR texmacs_opendir(string dirname) {
  QDirIterator *iterator = new QDirIterator(
    texmacs_string_to_qstring(dirname),
    QDir::Files | QDir::Dirs | QDir::NoDotAndDotDot
  );
  if (!iterator->hasNext()) {
    delete iterator;
    return nullptr;
  }
  texmacs_android_dir_t *dir = new texmacs_android_dir_t;
  dir->iterator = iterator;
  dir->dirname = texmacs_string_to_qstring(dirname);
  return (TEXMACS_DIR)dir;
}

void texmacs_closedir(TEXMACS_DIR dir) {
  texmacs_android_dir_t *dirp = (texmacs_android_dir_t*)dir;
  delete dirp->iterator;
  delete dirp;
}

texmacs_dirent texmacs_readdir(TEXMACS_DIR dirp) {
  texmacs_android_dir_t *dir = (texmacs_android_dir_t*)dirp;
  if (!dir->iterator->hasNext()) {
    return {false, ""};
  }
  return {true, texmacs_qstring_to_string(dir->iterator->next())};
}

int texmacs_stat(string filename, struct stat *buf) {
    QFileInfo info(texmacs_string_to_qstring(filename));
    
    if (!info.exists()) {
        return -1;
    }

    // File type
    buf->st_mode = (info.isDir() ? S_IFDIR : S_IFREG);
    buf->st_mode |= (info.isExecutable() ? S_IXUSR : 0);

    // get permissions
    QFile::Permissions perms = info.permissions();
    
    buf->st_mode |= (perms & QFile::ReadOwner) ? S_IRUSR : 0;
    buf->st_mode |= (perms & QFile::WriteOwner) ? S_IWUSR : 0;
    buf->st_mode |= (perms & QFile::ExeOwner) ? S_IXUSR : 0;

    buf->st_mode |= (perms & QFile::ReadGroup) ? S_IRGRP : 0;
    buf->st_mode |= (perms & QFile::WriteGroup) ? S_IWGRP : 0;
    buf->st_mode |= (perms & QFile::ExeGroup) ? S_IXGRP : 0;

    buf->st_mode |= (perms & QFile::ReadOther) ? S_IROTH : 0;
    buf->st_mode |= (perms & QFile::WriteOther) ? S_IWOTH : 0;
    buf->st_mode |= (perms & QFile::ExeOther) ? S_IXOTH : 0;

    if (!info.isDir()) {
      buf->st_size = info.size();
    } else {
      buf->st_size = 4096;
    }

  #if QT_VERSION >= 0x060000
    buf->st_mtime = info.lastModified().toSecsSinceEpoch();
    buf->st_atime = info.lastRead().toSecsSinceEpoch();
    buf->st_ctime = info.birthTime().toSecsSinceEpoch();
  #else
    buf->st_mtime = info.lastModified().toTime_t();
    buf->st_atime = info.lastRead().toTime_t();
    buf->st_ctime = info.created().toTime_t();
  #endif
    return 0;
}

bool texmacs_mkdir(string dirname, int mode) {
  return QDir().mkdir(texmacs_string_to_qstring(dirname));
}

bool texmacs_rmdir(string dirname) {
  return QDir().rmdir(texmacs_string_to_qstring(dirname));
}

bool texmacs_rename(string oldname, string newname) {
  return QFile::rename(
    texmacs_string_to_qstring(oldname),
    texmacs_string_to_qstring(newname)
  );
}

bool texmacs_chmod(string filename, int mode) {
  // do nothing
}

bool texmacs_remove(string filename) {
  QString homePath = QDir::homePath();
  if (!texmacs_string_to_qstring(filename).startsWith(homePath)) {
    qWarning() << "Refusing to delete file not in home directory";
    return false;
  }
  return QFile::remove(texmacs_string_to_qstring(filename));
}

bool texmacs_getenv(string variable_name, string &variable_value) {
  QByteArray value = qgetenv(
    texmacs_string_to_qstring(variable_name).toUtf8()
  );
  if (value.isEmpty()) {
    return false;
  }
  variable_value = texmacs_qbytearray_to_string(value);
  return true;
}

bool texmacs_setenv(string variable_name, string new_value) {
  return qputenv(
    texmacs_string_to_qstring(variable_name).toUtf8(),
    texmacs_string_to_qstring(new_value).toUtf8()
  );
}

string get_default_theme() {
#if QT_VERSION >= 0x060000
  if (QGuiApplication::styleHints()->colorScheme() == Qt::ColorScheme::Dark) {
    return "dark";
  } else {
    return "light";
  }
#else
  return "light";
#endif
}

int texmacs_guile_printf(const char *format, ...) {
  va_list args;
  va_start(args, format);
  QString output = QString::vasprintf(format, args);
  va_end(args);
  qDebug() << output;
  return output.size();
}

int texmacs_guile_fprintf(FILE *stream, const char *format, ...) {
  if (stream != stdout && stream != stderr) {
    va_list args;
    va_start(args, format);
    vfprintf(stream, format, args);
    va_end(args);
    return 0;
  }

  va_list args;
  va_start(args, format);
  QString output = QString::vasprintf(format, args);
  va_end(args);
  
  if (stream == stdout) {
    qDebug() << output;
  } else if (stream == stderr) {
    qWarning() << output;
    string s = texmacs_qstring_to_string(output);
    std_warning << s << "\n";
  }
  
  return output.size();
}

void texmacs_guile_log(const char *cmsg, int len)
{
    QString msg = QString::fromStdString(std::string(cmsg, len));
    qDebug().noquote() << msg;
    
    string s = string(cmsg, len);
    std_warning << s << "\n";
}

url texmacs_get_application_directory() {
  QString home = QDir::homePath();
  return url_system(texmacs_qstring_to_string(home));
}

bool is_doing_long_task = false;
using time_point = std::chrono::time_point<std::chrono::system_clock>;
using duration = std::chrono::duration<double>;

void texmacs_system_start_long_task() {
  if (is_doing_long_task) return;
  is_doing_long_task = true;
#ifdef QTTEXMACS
  QGuiApplication::setOverrideCursor(QCursor(Qt::WaitCursor));
#endif
}
void texmacs_system_end_long_task() {
  if (!is_doing_long_task) return;
  is_doing_long_task = false;
#ifdef QTTEXMACS
  
  QGuiApplication::restoreOverrideCursor();  
  QApplication::alert(QApplication::topLevelWidgets().first());
#endif
}

void texmacs_process_event() {
  if (!is_doing_long_task) return;
  static time_point last_time = std::chrono::system_clock::now();
  time_point current_time = std::chrono::system_clock::now();
  duration elapsed_seconds = current_time - last_time;
  if (elapsed_seconds.count() < 0.1) return;
  last_time = current_time;
#ifdef QTTEXMACS
  QCoreApplication::processEvents(QEventLoop::ExcludeUserInputEvents);
#endif
}

void texmacs_init_guile_hooks() {
  guile_fprintf = texmacs_guile_fprintf;
  guile_printf = texmacs_guile_printf;
  guile_process_event = texmacs_process_event;
  scm_set_log_function(texmacs_guile_log);
}