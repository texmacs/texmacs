package org.texmacs;

import android.Manifest;
import android.app.Activity;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.os.Environment;
import android.provider.Settings;
import android.widget.Button;
import android.widget.LinearLayout;
import android.widget.Toast;

public class PermissionsActivity extends Activity {

    private static final int NOTIFICATION_REQ_CODE = 101;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        // Programmatically create the UI to avoid needing XML layout resources
        LinearLayout layout = new LinearLayout(this);
        layout.setOrientation(LinearLayout.VERTICAL);
        layout.setPadding(64, 64, 64, 64);

        Button btnNotification = new Button(this);
        btnNotification.setText("Ask Notification Permission");
        btnNotification.setOnClickListener(v -> requestNotificationPermission());

        Button btnStorage = new Button(this);
        btnStorage.setText("Ask Storage Permission");
        btnStorage.setOnClickListener(v -> requestStoragePermission());

        Button btnLaunch = new Button(this);
        btnLaunch.setText("Launch TeXmacs");
        btnLaunch.setOnClickListener(v -> launchQtApp());

        layout.addView(btnNotification);
        layout.addView(btnStorage);
        layout.addView(btnLaunch);

        setContentView(layout);
    }

    private void requestNotificationPermission() {
        // Notifications require explicit runtime permission starting in Android 13 (API 33)
        if (Build.VERSION.SDK_INT >= 33) {
            if (checkSelfPermission(Manifest.permission.POST_NOTIFICATIONS) != PackageManager.PERMISSION_GRANTED) {
                requestPermissions(new String[]{Manifest.permission.POST_NOTIFICATIONS}, NOTIFICATION_REQ_CODE);
            } else {
                Toast.makeText(this, "Notification permission is already granted.", Toast.LENGTH_SHORT).show();
            }
        } else {
            Toast.makeText(this, "Notifications do not require runtime permission on this Android version.", Toast.LENGTH_SHORT).show();
        }
    }

    private void requestStoragePermission() {
        // MANAGE_EXTERNAL_STORAGE is required for all files access starting in Android 11 (API 30)
        if (Build.VERSION.SDK_INT >= 30) {
            if (!Environment.isExternalStorageManager()) {
                try {
                    Intent intent = new Intent(Settings.ACTION_MANAGE_APP_ALL_FILES_ACCESS_PERMISSION);
                    intent.addCategory("android.intent.category.DEFAULT");
                    intent.setData(Uri.parse(String.format("package:%s", getPackageName())));
                    startActivity(intent);
                } catch (Exception e) {
                    Intent intent = new Intent(Settings.ACTION_MANAGE_ALL_FILES_ACCESS_PERMISSION);
                    startActivity(intent);
                }
            } else {
                Toast.makeText(this, "Storage permission is already granted.", Toast.LENGTH_SHORT).show();
            }
        }
    }

    private void launchQtApp() {
        Intent intent = new Intent(this, org.qtproject.qt.android.bindings.QtActivity.class);
        // Forward any extras that might have launched the app (e.g., file associations)
        if (getIntent() != null && getIntent().getExtras() != null) {
            intent.putExtras(getIntent().getExtras());
        }
        startActivity(intent);
        
        // Finish this activity so pressing the 'Back' button in Qt doesn't return to this permission screen
        finish();
    }
}