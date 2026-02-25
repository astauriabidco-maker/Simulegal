const CACHE_NAME = 'simulegal-v1';
const STATIC_ASSETS = [
    '/',
    '/admin/sales',
    '/manifest.json',
];

// ─── Installation : Précache des assets statiques ───
self.addEventListener('install', (event) => {
    event.waitUntil(
        caches.open(CACHE_NAME).then((cache) => {
            console.log('[SW] Pre-caching static assets');
            return cache.addAll(STATIC_ASSETS);
        })
    );
    self.skipWaiting();
});

// ─── Activation : Nettoyage des anciens caches ───
self.addEventListener('activate', (event) => {
    event.waitUntil(
        caches.keys().then((keys) => {
            return Promise.all(
                keys
                    .filter((key) => key !== CACHE_NAME)
                    .map((key) => caches.delete(key))
            );
        })
    );
    self.clients.claim();
});

// ─── Fetch : Stratégies de cache ───
self.addEventListener('fetch', (event) => {
    const { request } = event;
    const url = new URL(request.url);

    // Ne pas cacher les requêtes API (toujours réseau)
    if (url.pathname.startsWith('/api') || url.pathname.startsWith('/sales') ||
        url.pathname.startsWith('/payments') || url.pathname.startsWith('/lead-capture')) {
        // Network-first pour les API
        event.respondWith(
            fetch(request)
                .then((response) => {
                    // Cacher les réponses GET réussies en background
                    if (request.method === 'GET' && response.status === 200) {
                        const responseClone = response.clone();
                        caches.open(CACHE_NAME).then((cache) => {
                            cache.put(request, responseClone);
                        });
                    }
                    return response;
                })
                .catch(() => {
                    // Fallback au cache si offline
                    return caches.match(request).then((cached) => {
                        if (cached) return cached;
                        // Retourner une réponse offline si rien en cache
                        return new Response(
                            JSON.stringify({ offline: true, message: 'Vous êtes hors ligne. Données en cache affichées.' }),
                            { headers: { 'Content-Type': 'application/json' }, status: 503 }
                        );
                    });
                })
        );
        return;
    }

    // Cache-first pour les assets statiques (CSS, JS, images)
    if (request.method === 'GET' && (
        url.pathname.match(/\.(js|css|png|jpg|jpeg|svg|ico|woff2?)$/) ||
        url.pathname.startsWith('/_next/')
    )) {
        event.respondWith(
            caches.match(request).then((cached) => {
                if (cached) return cached;
                return fetch(request).then((response) => {
                    const responseClone = response.clone();
                    caches.open(CACHE_NAME).then((cache) => {
                        cache.put(request, responseClone);
                    });
                    return response;
                });
            })
        );
        return;
    }

    // Stale-while-revalidate pour les pages HTML
    event.respondWith(
        caches.match(request).then((cached) => {
            const fetchPromise = fetch(request).then((response) => {
                if (response.status === 200) {
                    const responseClone = response.clone();
                    caches.open(CACHE_NAME).then((cache) => {
                        cache.put(request, responseClone);
                    });
                }
                return response;
            }).catch(() => cached);

            return cached || fetchPromise;
        })
    );
});

// ─── Background Sync : Sauvegarder les actions offline ───
self.addEventListener('sync', (event) => {
    if (event.tag === 'sync-prospects') {
        event.waitUntil(syncOfflineActions());
    }
});

async function syncOfflineActions() {
    // Récupérer les actions en attente depuis IndexedDB
    // et les rejouer quand la connexion revient
    console.log('[SW] Syncing offline actions...');
}

// ─── Notifications Push (prêt pour le futur) ───
self.addEventListener('push', (event) => {
    if (!event.data) return;
    const payload = event.data.json();
    event.waitUntil(
        self.registration.showNotification(payload.title || 'SimuLegal', {
            body: payload.body || 'Nouvelle notification',
            icon: '/icons/icon-192.png',
            badge: '/icons/icon-192.png',
            data: payload.data || {},
            actions: payload.actions || [],
        })
    );
});

self.addEventListener('notificationclick', (event) => {
    event.notification.close();
    const url = event.notification.data?.url || '/admin/sales';
    event.waitUntil(
        self.clients.matchAll({ type: 'window' }).then((clients) => {
            for (const client of clients) {
                if (client.url.includes(url) && 'focus' in client) return client.focus();
            }
            return self.clients.openWindow(url);
        })
    );
});
