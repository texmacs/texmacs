package org.texmacs;

import android.app.Activity;
import android.content.Intent;
import android.graphics.Color;
import android.graphics.Typeface;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.view.Gravity;
import android.view.Window;
import android.widget.LinearLayout;
import android.widget.ProgressBar;
import android.widget.TextView;
import android.widget.Toast;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class ExtractionActivity extends Activity {

    private ProgressBar progressBar;
    private TextView textStatus;
    private TextView textFilename;

    // Chemin de base où extraire les fichiers (à adapter selon les besoins de TeXmacs)
    // getFilesDir() pointe vers /data/user/0/org.texmacs/files/
    private String destBasePath;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        requestWindowFeature(Window.FEATURE_NO_TITLE);
        super.onCreate(savedInstanceState);

        destBasePath = getFilesDir().getAbsolutePath() + "/";

        setupUI();
        startExtraction();
    }

    /**
     * Construit l'interface utilisateur avec une barre de progression
     */
    private void setupUI() {
        LinearLayout rootLayout = new LinearLayout(this);
        rootLayout.setOrientation(LinearLayout.VERTICAL);
        rootLayout.setPadding(80, 80, 80, 80);
        rootLayout.setBackgroundColor(Color.parseColor("#F8F9FA"));
        rootLayout.setGravity(Gravity.CENTER);

        TextView textTitle = new TextView(this);
        textTitle.setText("Initialisation de TeXmacs");
        textTitle.setTextSize(24);
        textTitle.setTypeface(null, Typeface.BOLD);
        textTitle.setTextColor(Color.parseColor("#212529"));
        textTitle.setGravity(Gravity.CENTER_HORIZONTAL);
        rootLayout.addView(textTitle);

        textStatus = new TextView(this);
        textStatus.setText("Préparation de l'extraction...");
        textStatus.setTextSize(16);
        textStatus.setTextColor(Color.parseColor("#495057"));
        textStatus.setPadding(0, 32, 0, 16);
        rootLayout.addView(textStatus);

        // Barre de progression horizontale
        progressBar = new ProgressBar(this, null, android.R.attr.progressBarStyleHorizontal);
        progressBar.setIndeterminate(false);
        progressBar.setMax(100);
        progressBar.setProgress(0);
        LinearLayout.LayoutParams params = new LinearLayout.LayoutParams(
                LinearLayout.LayoutParams.MATCH_PARENT,
                LinearLayout.LayoutParams.WRAP_CONTENT
        );
        params.setMargins(0, 16, 0, 16);
        progressBar.setLayoutParams(params);
        rootLayout.addView(progressBar);

        textFilename = new TextView(this);
        textFilename.setText("");
        textFilename.setTextSize(12);
        textFilename.setTextColor(Color.parseColor("#6C757D"));
        textFilename.setGravity(Gravity.CENTER_HORIZONTAL);
        rootLayout.addView(textFilename);

        setContentView(rootLayout);
    }

    /**
     * Lance le processus d'extraction dans un thread secondaire
     */
    private void startExtraction() {
        ExecutorService executor = Executors.newSingleThreadExecutor();
        Handler handler = new Handler(Looper.getMainLooper());

        executor.execute(() -> {
            try {
                InputStream is = getAssets().open("raw/resource_files.txt");
                BufferedReader reader = new BufferedReader(new InputStreamReader(is));

                List<String> lines = new ArrayList<>();
                String line;
                while ((line = reader.readLine()) != null) {
                    if (!line.trim().isEmpty()) {
                        lines.add(line.trim());
                    }
                }
                reader.close();

                int totalFiles = lines.size();
                android.util.Log.i("TeXmacsExtract", "Fichier resource_files.txt lu. Nombre de lignes : " + totalFiles);

                if (totalFiles == 0) {
                    throw new Exception("resource_files.txt est vide ou introuvable.");
                }

                int filesExtracted = 0;

                for (int i = 0; i < totalFiles; i++) {
                    String[] parts = lines.get(i).split("\\s+");

                    if (parts.length < 2) {
                        android.util.Log.w("TeXmacsExtract", "Ligne mal formatée ignorée : '" + lines.get(i) + "'");
                        continue;
                    }

                    String id = parts[0];
                    String filename = parts[1];

                    final int currentProgress = (int) (((i + 1) / (float) totalFiles) * 100);
                    int finalI = i;
                    handler.post(() -> {
                        progressBar.setProgress(currentProgress);
                        textStatus.setText("Extraction (" + (finalI + 1) + "/" + totalFiles + ")");
                        if (filename.length() > 40) {
                            textFilename.setText("..." + filename.substring(filename.length() - 40));
                        } else {
                            textFilename.setText(filename);
                        }
                    });

                    extractSingleFile(id, filename);
                    filesExtracted++;
                }

                android.util.Log.i("TeXmacsExtract", "Extraction terminée. Fichiers extraits : " + filesExtracted);

                handler.post(() -> {
                    progressBar.setProgress(100);
                    textStatus.setText("Extraction terminée !");
                    launchNextActivity();
                });

            } catch (Exception e) {
                e.printStackTrace();
                android.util.Log.e("TeXmacsExtract", "CRASH PENDANT L'EXTRACTION", e);
                final String errorMsg = e.getMessage();
                handler.post(() -> {
                    textStatus.setText("Erreur d'extraction");
                    textStatus.setTextColor(Color.RED);
                    textFilename.setText(errorMsg);
                });
            }
        });
    }

    /**
     * Lit un fichier depuis les assets et l'écrit sur le stockage
     */
    private void extractSingleFile(String id, String relativeFilename) throws Exception {
        String fullPath = destBasePath + relativeFilename;
        File outFile = new File(fullPath);

        // Création des dossiers parents de manière récursive
        File parentDir = outFile.getParentFile();
        if (parentDir != null && !parentDir.exists()) {
            parentDir.mkdirs();
        }

        // Lecture depuis assets:raw/{id}.raw
        InputStream in = getAssets().open("raw/" + id + ".raw");
        OutputStream out = new FileOutputStream(outFile);

        byte[] buffer = new byte[8192];
        int read;
        while ((read = in.read(buffer)) != -1) {
            out.write(buffer, 0, read);
        }

        in.close();
        out.flush();
        out.close();
    }

    /**
     * Lance l'application Qt principale
     */
    private void launchNextActivity() {
        Intent intent = new Intent(this, org.qtproject.qt.android.bindings.QtActivity.class);
        
        if (getIntent() != null && getIntent().getExtras() != null) {
            intent.putExtras(getIntent().getExtras());
        }
        
        startActivity(intent);
        
        finish(); 
    }
}