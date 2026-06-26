package org.texmacs;

import android.app.Notification;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.app.Service;
import android.content.Intent;
import android.content.pm.ServiceInfo;
import android.os.Build;
import android.os.IBinder;

public class TexmacsService extends Service {
    private static final String CHANNEL_ID = "TexmacsServiceChannel";
    private static final int NOTIFICATION_ID = 1;
    private static final String ACTION_QUIT = "ACTION_QUIT_TEXMACS";
    
    private static TexmacsService instance = null; 
    
    private NotificationManager notificationManager;

    @Override
    public void onCreate() {
        super.onCreate();
        instance = this;
        notificationManager = getSystemService(NotificationManager.class);
        createNotificationChannel();
        
        // 1. Intent pour le bouton "Ouvrir TeXmacs"
        Intent openIntent = new Intent(this, org.qtproject.qt.android.bindings.QtActivity.class);
        // FLAG_ACTIVITY_SINGLE_TOP évite de recréer l'activité si elle est déjà ouverte
        openIntent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_SINGLE_TOP);
        PendingIntent pendingOpenIntent = PendingIntent.getActivity(
                this, 0, openIntent, PendingIntent.FLAG_IMMUTABLE | PendingIntent.FLAG_UPDATE_CURRENT
        );

        // 2. Intent pour le bouton "Quitter TeXmacs"
        Intent quitIntent = new Intent(this, TexmacsService.class);
        quitIntent.setAction(ACTION_QUIT);
        PendingIntent pendingQuitIntent = PendingIntent.getService(
                this, 0, quitIntent, PendingIntent.FLAG_IMMUTABLE | PendingIntent.FLAG_UPDATE_CURRENT
        );

        // 3. Construction des actions
        Notification.Action openAction = new Notification.Action.Builder(
                android.R.drawable.ic_menu_view, "Ouvrir TeXmacs", pendingOpenIntent).build();
                
        Notification.Action quitAction = new Notification.Action.Builder(
                android.R.drawable.ic_menu_close_clear_cancel, "Quitter", pendingQuitIntent).build();

        // 4. Construction de la notification avec les actions
        Notification.Builder notificationBuilder = new Notification.Builder(this, CHANNEL_ID)
                .setContentTitle("TeXmacs")
                .setContentText("TeXmacs en cours d'exécution...")
                .setSmallIcon(android.R.drawable.ic_popup_sync) 
                .setOnlyAlertOnce(true)
                .setContentIntent(pendingOpenIntent) // Action par défaut si on clique sur la notification elle-même
                .addAction(openAction)
                .addAction(quitAction);

        // Lancement du service au premier plan
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.Q) {
            startForeground(NOTIFICATION_ID, notificationBuilder.build(), ServiceInfo.FOREGROUND_SERVICE_TYPE_DATA_SYNC);
        } else {
            startForeground(NOTIFICATION_ID, notificationBuilder.build());
        }
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        // Si l'intention reçue est notre action de quitter, on arrête tout
        if (intent != null && ACTION_QUIT.equals(intent.getAction())) {
            // Arrête la notification persistante
            stopForeground(true);
            // Arrête le service
            stopSelf();
            
            android.os.Process.killProcess(android.os.Process.myPid());
            
            return START_NOT_STICKY;
        }
        
        return START_STICKY;
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        instance = null;
    }

    @Override
    public IBinder onBind(Intent intent) {
        return null; 
    }

    private void createNotificationChannel() {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            NotificationChannel serviceChannel = new NotificationChannel(
                    CHANNEL_ID,
                    "Service TeXmacs",
                    NotificationManager.IMPORTANCE_LOW
            );
            if (notificationManager != null) {
                notificationManager.createNotificationChannel(serviceChannel);
            }
        }
    }
}