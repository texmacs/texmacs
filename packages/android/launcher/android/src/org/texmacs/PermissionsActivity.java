package org.texmacs;

import android.Manifest;
import android.app.Activity;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.graphics.Color;
import android.graphics.Typeface;
import android.graphics.drawable.GradientDrawable;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.os.Environment;
import android.provider.Settings;
import android.view.Gravity;
import android.view.View;
import android.view.Window;
import android.widget.Button;
import android.widget.LinearLayout;
import android.widget.TextView;
import android.widget.Toast;

public class PermissionsActivity extends Activity {

    private static final int NOTIFICATION_REQ_CODE = 101;
    private static final int STORAGE_REQ_CODE = 102;
    private boolean isLaunching = false;

    // Design Colors
    private static final int COLOR_BACKGROUND = Color.parseColor("#F8F9FA");
    private static final int COLOR_TEXT_TITLE = Color.parseColor("#212529");
    private static final int COLOR_TEXT_BODY = Color.parseColor("#495057");
    private static final int COLOR_BTN_PRIMARY = Color.parseColor("#0D6EFD"); 
    private static final int COLOR_BTN_LAUNCH = Color.parseColor("#198754");  
    private static final int COLOR_BTN_DISABLED = Color.parseColor("#ADB5BD");

    // UI Elements that need dynamic updates
    private LinearLayout notifSection;
    private LinearLayout storageSection;
    private Button btnLaunch;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        requestWindowFeature(Window.FEATURE_NO_TITLE);
        super.onCreate(savedInstanceState);

        if (hasNotificationPermission() && hasStoragePermission()) {
            launchQtApp();
            return;
        }

        // --- Main Container ---
        LinearLayout rootLayout = new LinearLayout(this);
        rootLayout.setOrientation(LinearLayout.VERTICAL);
        rootLayout.setPadding(80, 80, 80, 80);
        rootLayout.setBackgroundColor(COLOR_BACKGROUND);
        rootLayout.setGravity(Gravity.CENTER_VERTICAL);

        // --- Title ---
        TextView textTitle = new TextView(this);
        textTitle.setText("Welcome to TeXmacs for Android 🩷");
        textTitle.setTextSize(26);
        textTitle.setTypeface(null, Typeface.BOLD);
        textTitle.setTextColor(COLOR_TEXT_TITLE);
        textTitle.setGravity(Gravity.CENTER_HORIZONTAL);
        setMargins(textTitle, 0, 0, 0, 16);
        rootLayout.addView(textTitle);

        // --- Introduction ---
        TextView textIntro = new TextView(this);
        textIntro.setText("To function correctly, the application requires the following permissions.");
        textIntro.setTextSize(16);
        textIntro.setTextColor(COLOR_TEXT_BODY);
        textIntro.setGravity(Gravity.CENTER_HORIZONTAL);
        setMargins(textIntro, 0, 0, 0, 64);
        rootLayout.addView(textIntro);

        // --- Notifications Section (Container) ---
        notifSection = new LinearLayout(this);
        notifSection.setOrientation(LinearLayout.VERTICAL);
        
        TextView textNotifTitle = new TextView(this);
        textNotifTitle.setText("Notifications");
        textNotifTitle.setTypeface(null, Typeface.BOLD);
        textNotifTitle.setTextColor(COLOR_TEXT_TITLE);
        notifSection.addView(textNotifTitle);

        TextView textNotifExplanation = new TextView(this);
        textNotifExplanation.setText("Keeps TeXmacs active in the background to secure your unsaved documents.");
        textNotifExplanation.setTextColor(COLOR_TEXT_BODY);
        setMargins(textNotifExplanation, 0, 8, 0, 16);
        notifSection.addView(textNotifExplanation);

        Button btnNotification = createStyledButton("Allow Notifications", COLOR_BTN_PRIMARY);
        btnNotification.setOnClickListener(v -> requestNotificationPermission());
        notifSection.addView(btnNotification);
        
        rootLayout.addView(notifSection);

        // --- Storage Section (Container) ---
        storageSection = new LinearLayout(this);
        storageSection.setOrientation(LinearLayout.VERTICAL);
        setMargins(storageSection, 0, 48, 0, 0);

        TextView textStorageTitle = new TextView(this);
        textStorageTitle.setText("File Access");
        textStorageTitle.setTypeface(null, Typeface.BOLD);
        textStorageTitle.setTextColor(COLOR_TEXT_TITLE);
        storageSection.addView(textStorageTitle);

        TextView textStorageExplanation = new TextView(this);
        textStorageExplanation.setText("Allows reading, modifying, and saving your documents on your device.");
        textStorageExplanation.setTextColor(COLOR_TEXT_BODY);
        setMargins(textStorageExplanation, 0, 8, 0, 16);
        storageSection.addView(textStorageExplanation);

        Button btnStorage = createStyledButton("Allow Storage Access", COLOR_BTN_PRIMARY);
        btnStorage.setOnClickListener(v -> requestStoragePermission());
        storageSection.addView(btnStorage);

        rootLayout.addView(storageSection);

        // --- Launch Button ---
        btnLaunch = createStyledButton("Launch TeXmacs", COLOR_BTN_DISABLED);
        setMargins(btnLaunch, 0, 80, 0, 0);
        btnLaunch.setOnClickListener(v -> {
            if (hasNotificationPermission() && hasStoragePermission()) {
                launchQtApp();
            }
        });
        rootLayout.addView(btnLaunch);

        setContentView(rootLayout);
        
        // Initial UI state setup
        updateUI();
    }

    /**
     * Updates the UI state: hides granted permissions and manages the Launch button.
     */
    private void updateUI() {
        boolean hasNotif = hasNotificationPermission();
        boolean hasStorage = hasStoragePermission();

        // Hide or show sections based on permission status
        if (notifSection != null) {
            notifSection.setVisibility(hasNotif ? View.GONE : View.VISIBLE);
        }
        if (storageSection != null) {
            storageSection.setVisibility(hasStorage ? View.GONE : View.VISIBLE);
        }

        // Grey out or enable the Launch button
        if (btnLaunch != null) {
            GradientDrawable shape = (GradientDrawable) btnLaunch.getBackground();
            if (hasNotif && hasStorage) {
                btnLaunch.setEnabled(true);
                shape.setColor(COLOR_BTN_LAUNCH);
            } else {
                btnLaunch.setEnabled(false);
                shape.setColor(COLOR_BTN_DISABLED);
            }
        }
    }

    @Override
    protected void onResume() {
        super.onResume();
        // Triggered when returning from Android Settings (e.g. Storage Manager)
        if (!isLaunching) {
            updateUI();
            if (hasNotificationPermission() && hasStoragePermission()) {
                launchQtApp();
            }
        }
    }

    @Override
    public void onRequestPermissionsResult(int requestCode, String[] permissions, int[] grantResults) {
        super.onRequestPermissionsResult(requestCode, permissions, grantResults);
        // Triggered immediately after standard permission popups
        updateUI();
        if (hasNotificationPermission() && hasStoragePermission()) {
            launchQtApp();
        }
    }

    // --- Permission Checks ---

    private boolean hasNotificationPermission() {
        if (Build.VERSION.SDK_INT >= 33) {
            return checkSelfPermission(Manifest.permission.POST_NOTIFICATIONS) == PackageManager.PERMISSION_GRANTED;
        }
        return true; // Not required on older versions
    }

    private boolean hasStoragePermission() {
        if (Build.VERSION.SDK_INT >= 30) {
            return Environment.isExternalStorageManager();
        } else if (Build.VERSION.SDK_INT >= 23) {
            return checkSelfPermission(Manifest.permission.WRITE_EXTERNAL_STORAGE) == PackageManager.PERMISSION_GRANTED;
        }
        return true;
    }

    // --- Permission Requests ---

    private void requestNotificationPermission() {
        if (Build.VERSION.SDK_INT >= 33) {
            requestPermissions(new String[]{Manifest.permission.POST_NOTIFICATIONS}, NOTIFICATION_REQ_CODE);
        }
    }

    private void requestStoragePermission() {
        if (Build.VERSION.SDK_INT >= 30) {
            try {
                Intent intent = new Intent(Settings.ACTION_MANAGE_APP_ALL_FILES_ACCESS_PERMISSION);
                intent.addCategory("android.intent.category.DEFAULT");
                intent.setData(Uri.parse(String.format("package:%s", getPackageName())));
                startActivity(intent);
            } catch (Exception e) {
                Intent intent = new Intent(Settings.ACTION_MANAGE_ALL_FILES_ACCESS_PERMISSION);
                startActivity(intent);
            }
        } else if (Build.VERSION.SDK_INT >= 23) {
            requestPermissions(new String[]{Manifest.permission.READ_EXTERNAL_STORAGE, Manifest.permission.WRITE_EXTERNAL_STORAGE}, STORAGE_REQ_CODE);
        }
    }

    // --- Utility Methods ---

    private Button createStyledButton(String text, int backgroundColor) {
        Button btn = new Button(this);
        btn.setText(text);
        btn.setTextColor(Color.WHITE);
        btn.setTextSize(16);
        btn.setTypeface(null, Typeface.BOLD);
        btn.setTransformationMethod(null); 
        btn.setPadding(0, 32, 0, 32);

        GradientDrawable shape = new GradientDrawable();
        shape.setShape(GradientDrawable.RECTANGLE);
        shape.setCornerRadius(24f); 
        shape.setColor(backgroundColor);
        btn.setBackground(shape);

        return btn;
    }

    private void setMargins(View view, int left, int top, int right, int bottom) {
        if (view.getLayoutParams() instanceof LinearLayout.LayoutParams) {
            LinearLayout.LayoutParams params = (LinearLayout.LayoutParams) view.getLayoutParams();
            params.setMargins(left, top, right, bottom);
            view.requestLayout();
        } else {
            LinearLayout.LayoutParams params = new LinearLayout.LayoutParams(
                    LinearLayout.LayoutParams.MATCH_PARENT,
                    LinearLayout.LayoutParams.WRAP_CONTENT
            );
            params.setMargins(left, top, right, bottom);
            view.setLayoutParams(params);
        }
    }

    private void launchQtApp() {
        if (isLaunching) return;
        isLaunching = true;

        Intent intent = new Intent(this, org.qtproject.qt.android.bindings.QtActivity.class);
        if (getIntent() != null && getIntent().getExtras() != null) {
            intent.putExtras(getIntent().getExtras());
        }
        startActivity(intent);
        finish();
    }
}