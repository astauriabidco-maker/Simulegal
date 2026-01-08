# 📚 Documentation API Simulegal

> API REST pour l'évaluation d'éligibilité aux titres de séjour en France.

**Base URL** : `http://localhost:8001`  
**Documentation Interactive** : [/docs](http://localhost:8001/docs) (Swagger UI)

---

## 🔐 Authentification

L'API utilise **OAuth2 avec Bearer Token**. Obtenez un token via l'endpoint `/api/auth/token`.

### Headers Requis (endpoints protégés)
```
Authorization: Bearer <access_token>
```

---

## 📋 Endpoints

### 🔑 Auth (`/api/auth`)

| Méthode | Endpoint | Auth | Description |
|:---|:---|:---:|:---|
| `POST` | `/api/auth/register` | ❌ | Création de compte |
| `POST` | `/api/auth/token` | ❌ | Connexion (obtenir token) |
| `GET` | `/api/auth/me` | ✅ | Informations utilisateur courant |

#### POST `/api/auth/register`
```json
// Request
{
  "email": "user@example.com",
  "password": "securepass123",
  "full_name": "Jean Dupont"
}

// Response 200
{
  "id": 1,
  "email": "user@example.com",
  "full_name": "Jean Dupont",
  "role": "user"
}
```

#### POST `/api/auth/token`
```
// Form Data (x-www-form-urlencoded)
username=user@example.com
password=securepass123

// Response 200
{
  "access_token": "eyJhbGciOiJIUzI1NiIs...",
  "token_type": "bearer"
}
```

---

### 🎯 Éligibilité (`/api`)

| Méthode | Endpoint | Auth | Description |
|:---|:---|:---:|:---|
| `POST` | `/api/evaluate` | ⚪ Optional | Évaluer l'éligibilité |
| `GET` | `/api/simulation/{id}/results` | ❌ | Résultats simulation |
| `GET` | `/api/procedures` | ❌ | Liste des procédures |
| `GET` | `/api/procedures/{id}` | ❌ | Détails procédure |

#### POST `/api/evaluate`
```json
// Request
{
  "nationality": "Tunisienne",
  "residence_duration_months": 36,
  "french_level": "B1",
  "salary_annual": 35000,
  "spouse_nationality": "Française",
  "is_married": true,
  "community_of_life": true
}

// Response 200
{
  "simulation_id": 42,
  "results": [
    {
      "procedure_id": "resident_france_spouse",
      "name": "Carte de résident vie privée et familiale",
      "score": 100,
      "missing_criteria": [],
      "applied_exception": "tunisian_accord_spouse",
      "documents": ["Acte de mariage", "Justificatif de domicile"]
    }
  ],
  "top_match": { ... }
}
```

---

### 💬 Chat IA (`/api/chat`)

| Méthode | Endpoint | Auth | Description |
|:---|:---|:---:|:---|
| `POST` | `/api/chat/message` | ❌ | Envoyer un message au chatbot |
| `GET` | `/api/chat/advice/{simulation_id}` | ❌ | Obtenir conseils personnalisés |

#### POST `/api/chat/message`
```json
// Request
{
  "message": "Je suis tunisien, je vis en France depuis 5 ans",
  "simulation_id": null
}

// Response 200
{
  "reply": "Merci ! En êtes-vous marié(e) ?",
  "simulation_id": 43,
  "is_finished": false,
  "current_data": {
    "nationality": "Tunisienne",
    "residence_duration_months": 60
  }
}
```

---

### 💳 Paiement (`/api/payment`)

| Méthode | Endpoint | Auth | Description |
|:---|:---|:---:|:---|
| `POST` | `/api/payment/create-checkout/{simulation_id}` | ❌ | Créer session Stripe |
| `GET` | `/api/payment/verify-session/{session_id}` | ❌ | Vérifier paiement |

---

### 🔔 Notifications (`/api/notifications`)

| Méthode | Endpoint | Auth | Description |
|:---|:---|:---:|:---|
| `GET` | `/api/notifications/` | ✅ | Liste notifications |
| `GET` | `/api/notifications/unread-count` | ✅ | Nombre non-lues |
| `POST` | `/api/notifications/{id}/read` | ✅ | Marquer comme lue |
| `POST` | `/api/notifications/read-all` | ✅ | Tout marquer lu |
| `DELETE` | `/api/notifications/{id}` | ✅ | Supprimer notification |
| `DELETE` | `/api/notifications/` | ✅ | Supprimer toutes |

---

### 👔 Admin (`/api/admin`) 🔒

> Requiert rôle `admin`

| Méthode | Endpoint | Description |
|:---|:---|:---|
| `GET` | `/api/admin/simulations` | Liste toutes les simulations |
| `GET` | `/api/admin/stats` | Statistiques avancées |
| `GET` | `/api/admin/users` | Liste des utilisateurs |
| `POST` | `/api/admin/procedures` | Créer procédure |
| `PUT` | `/api/admin/procedures/{id}` | Modifier procédure |
| `DELETE` | `/api/admin/procedures/{id}` | Supprimer procédure |

#### GET `/api/admin/stats`
```json
// Response 200
{
  "total_simulations": 156,
  "paid_simulations": 42,
  "total_users": 89,
  "conversion_rate": 26.9,
  "revenue": 419.58,
  "this_week": 23,
  "avg_score": 72.5,
  "top_procedures": [
    {"name": "Carte de résident VPF", "count": 45}
  ],
  "daily_stats": [
    {"date": "2026-01-07", "label": "07/01", "count": 12}
  ]
}
```

---

### 📄 Rapports (`/api/reports`)

| Méthode | Endpoint | Auth | Description |
|:---|:---|:---:|:---|
| `GET` | `/api/reports/download/{simulation_id}` | ✅ | Télécharger PDF |

> ⚠️ Requiert que la simulation soit payée (`is_paid: true`)

---

## 🔧 Variables d'Environnement

| Variable | Description | Défaut |
|:---|:---|:---|
| `DATABASE_URL` | URL de connexion DB | `sqlite+aiosqlite:///./simulegal.db` |
| `JWT_SECRET` | Clé secrète JWT | `dev-secret-key` |
| `STRIPE_SECRET_KEY` | Clé API Stripe | - |
| `SMTP_HOST` | Serveur SMTP | `smtp.gmail.com` |
| `SMTP_USER` | Email SMTP | - |
| `SMTP_PASSWORD` | Mot de passe SMTP | - |
| `REPORT_PRICE` | Prix unitaire (€) | `9.99` |

---

## 📊 Codes d'Erreur

| Code | Description |
|:---|:---|
| `400` | Bad Request - Données invalides |
| `401` | Unauthorized - Token manquant/invalide |
| `403` | Forbidden - Permissions insuffisantes |
| `404` | Not Found - Ressource introuvable |
| `422` | Validation Error - Champs requis manquants |
| `500` | Internal Server Error |

---

## 🚀 Démarrage Rapide

```bash
# 1. Installer les dépendances
pip install -r requirements.txt

# 2. Lancer le serveur
./run_dev.sh

# 3. Accéder à la documentation
open http://localhost:8001/docs
```
