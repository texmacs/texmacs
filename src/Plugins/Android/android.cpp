/******************************************************************************
* MODULE   : android.cpp
* DESCRIPTION: android specific functions
* COPYRIGHT  : (C) 2024 Liza BELOS
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "android.hpp"
#include "android_system.hpp"

#include "tm_ostream.hpp"
#include "boot.hpp"
#include "file.hpp"
#include "tm_timer.hpp"
#include "data_cache.hpp"

#include <QApplication>
#include <QFile>
#include <QTextStream>
#include <QDir>
#include <QFileInfo>
#include <QStringList>
#include <QDebug>
#include <QDirIterator>
#include <QTimer>

#include <libguile.h>

void android_extract_from_asset(QString asset_path)
{
  // The resource text file contains the mapping between the path 
  // of the assets in the APK and the path where the files should
  // be extracted.
  QFile resource_files("assets:raw/resource_files.txt");
  if (!resource_files.open(QIODevice::ReadOnly | QIODevice::Text))
  {
    qDebug() << "Cannot open resource_files.txt";
    throw std::runtime_error("Cannot open resource_files.txt");
  }

  // Read the resource file line by line, and extract the files
  // into the right directory.
  QTextStream in(&resource_files);
  while (!in.atEnd())
  {
    // Read the line. The the first part is the id of the resource
    // and the second part is the name of the file (including the path)
    QString line = in.readLine();
    if (line == "") {
      continue;
    }
    // split the line in two parts
    QStringList parts = line.split(" ");
    if (parts.size() != 3)
      continue;
    QString id = parts[0];
    QString filename = parts[1];
    QString md5 = parts[2];
    qDebug() << "Extracting " << id << " to " << filename;

    // Read the file from the assets into the memory
    QFile file("assets:raw/" + id + ".raw");
    if (!file.open(QIODevice::ReadOnly))
      throw std::runtime_error("Cannot open " + id.toStdString() + ".raw");
    QByteArray data = file.readAll();
    file.close();
    
    // Create the directory (recursively)
    QDir dir;
    dir.mkpath(QFileInfo(filename).path());

    // Write the file to the disk
    QFile new_file(filename);
    if (!new_file.open(QIODevice::WriteOnly))
      throw std::runtime_error("Cannot open " + filename.toStdString());
    new_file.write(data);
    new_file.close();
  }
  resource_files.close();
}

void start_android_service() {
  QJniObject activity = QJniObject::callStaticObjectMethod("org/qtproject/qt/android/QtNative", "activity", "()Landroid/app/Activity;");
  if (!activity.isValid()) {
    QTimer::singleShot(1000, []() { 
      std_warning << "Cannot start Android service: activity is not valid\n";
    });
    return;
  }
  QJniObject intent("android/content/Intent");
  QJniObject className = QJniObject::fromString("org.texmacs.TexmacsService");
  
  intent.callObjectMethod(
      "setClassName", 
      "(Landroid/content/Context;Ljava/lang/String;)Landroid/content/Intent;", 
      activity.object(), 
      className.object<jstring>()
  );
                    
  QJniObject::callStaticMethod<void>(
      "androidx/core/content/ContextCompat", 
      "startForegroundService", 
      "(Landroid/content/Context;Landroid/content/Intent;)V", 
      activity.object(), 
      intent.object()
  );
}

void stop_android_service() {
  QJniObject activity = QJniObject::callStaticObjectMethod("org/qtproject/qt/android/QtNative", "activity", "()Landroid/app/Activity;");
  if (!activity.isValid()) {
    QTimer::singleShot(1000, []() { 
      std_warning << "Cannot stop Android service: activity is not valid\n";
    });
    return;
  }
  
  QJniObject intent("android/content/Intent");
  QJniObject className = QJniObject::fromString("org.texmacs.TexmacsService");
  
  // Configuration de l'Intent avec la classe du service
  intent.callObjectMethod(
      "setClassName", 
      "(Landroid/content/Context;Ljava/lang/String;)Landroid/content/Intent;", 
      activity.object(), 
      className.object<jstring>()
  );
                    
  // Appel de stopService(Intent) sur l'Activity (qui hérite de Context)
  jboolean stopped = activity.callMethod<jboolean>(
      "stopService", 
      "(Landroid/content/Intent;)Z", 
      intent.object()
  );
  
  if (!stopped) {
      std_warning << "Service was not running or could not be stopped.\n";
  }
}

void init_android()
{
  // android_extract_from_asset(QDir::homePath());
  start_android_service();
}
