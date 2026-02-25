import type { Metadata } from "next";
import "./globals.css";

export const metadata: Metadata = {
  title: "Simulegal — Assistance Juridique",
  description: "Plateforme CRM de prospection et gestion de dossiers juridiques",
  manifest: "/manifest.json",
  themeColor: "#4f46e5",
};

export default function RootLayout({
  children,
}: Readonly<{
  children: React.ReactNode;
}>) {
  return (
    <html lang="fr">
      <head>
        <link rel="manifest" href="/manifest.json" />
        <meta name="theme-color" content="#4f46e5" />
        <meta name="apple-mobile-web-app-capable" content="yes" />
        <meta name="apple-mobile-web-app-status-bar-style" content="black-translucent" />
        <meta name="apple-mobile-web-app-title" content="SimuLegal" />
      </head>
      <body className="font-sans antialiased">
        {children}
        {/* Service Worker Registration */}
        <script
          dangerouslySetInnerHTML={{
            __html: `
              if ('serviceWorker' in navigator) {
                window.addEventListener('load', () => {
                  navigator.serviceWorker.register('/sw.js')
                    .then(reg => console.log('[PWA] SW registered:', reg.scope))
                    .catch(err => console.log('[PWA] SW registration failed:', err));
                });
              }
            `,
          }}
        />
      </body>
    </html>
  );
}
