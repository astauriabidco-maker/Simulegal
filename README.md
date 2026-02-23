# SimuLegal — Plateforme SaaS de Franchise Juridique

> Simulateur d'éligibilité immigration + CRM commercial + Réseau de franchise pour l'accompagnement juridique en droit des étrangers.

## 🏗️ Architecture

| Couche      | Stack                          | Port  |
|-------------|--------------------------------|-------|
| Frontend    | Next.js 14 (App Router) + Tailwind v4 | 3000  |
| Backend     | NestJS + Prisma (SQLite)       | 3005  |

## 🚀 Démarrage rapide

```bash
# 1. Frontend
npm install
npm run dev

# 2. Backend (dans un second terminal)
cd backend
npm install
npx prisma generate
npx prisma db push
npm run start:dev
```

Ouvrir [http://localhost:3000](http://localhost:3000) pour le simulateur public.
Ouvrir [http://localhost:3000/staff-login](http://localhost:3000/staff-login) pour l'accès professionnel.

## 📦 Modules

### Cœur Métier
- **Simulateur d'éligibilité** — Moteur de règles JSON (séjour, naturalisation, famille, permis, asile) avec Wizard multi-étapes
- **Checkout** — Flow de paiement intégré avec Stripe (contrat, signature, paiement, RDV)
- **Espace Client** — Portail client avec upload de documents et scan guidé

### Pipeline Commercial (CRM)
- **Sales Dashboard** — Kanban de prospects avec tiroir de détail
- **Call Cockpit** — Intégration Twilio Voice SDK pour appels sortants
- **Booking** — Prise de RDV en agence avec confirmation SMS/WhatsApp
- **Analytics** — Dashboard analytique commercial
- **Lead Router** — Scoring et dispatch automatique des leads
- **Marketing Automation** — Emails/SMS déclenchés par changement de statut

### Réseau de Franchise
- **HQ Dashboard** — Vue réseau global avec carte de France interactive
- **Franchise Leads** — Pipeline candidats franchisés (conformité Loi Doubin / DIP)
- **Gestion des agences** — CRUD, paramétrage, fleet monitor de bornes/tablettes

### Administration
- **RBAC** — Système de rôles et permissions granulaires
- **Configuration** — Services, tarification, intégrations (Twilio, SMTP, Stripe), templates, automations pipeline
- **Veille juridique** — Suivi des évolutions légales avec audit trail
- **Finance** — Transactions, facturation, reversements aux franchisés

## 🔑 Variables d'environnement

### Backend (`backend/.env`)
```env
DATABASE_URL="file:./dev.db"
JWT_SECRET="your-jwt-secret"
STRIPE_SECRET_KEY="sk_test_..."
STRIPE_WEBHOOK_SECRET="whsec_..."
FRONTEND_URL="http://localhost:3000"
TWILIO_ACCOUNT_SID="..."
TWILIO_AUTH_TOKEN="..."
TWILIO_PHONE_NUMBER="..."
```

### Frontend (`.env.local`)
```env
NEXT_PUBLIC_API_URL=http://localhost:3005
NEXT_PUBLIC_STRIPE_PUBLISHABLE_KEY=pk_test_...
```

## 🗂️ Structure du projet

```
├── app/                    # Pages Next.js (App Router)
│   ├── admin/              # Back-office (18 sous-modules)
│   ├── staff-login/        # Connexion personnel
│   ├── espace-client/      # Portail client
│   └── page.tsx            # Landing + Simulateur public
├── backend/                # API NestJS
│   ├── prisma/             # Schema + migrations
│   └── src/                # Modules (26 modules)
├── components/             # Composants React
│   ├── admin/              # Dashboard, settings, finance
│   ├── backoffice/         # HQ, Agency, Eligibility config
│   ├── sales/              # CRM, Call cockpit, Analytics
│   ├── steps/              # Étapes du wizard simulateur
│   └── client/             # Portail client
├── data/                   # Règles d'éligibilité (JSON)
├── lib/                    # Moteur de règles + utilitaires
├── services/               # Stores frontend (30 services)
└── specs/                  # Spécifications métier
```

## 🧪 Tests

```bash
# Tests unitaires du moteur de règles
npm test

# Lint
npm run lint

# Build de production
npm run build
```

## 🐳 Docker

```bash
docker-compose up --build
```

## 📄 Licence

Propriétaire — © SimuLegal 2026. Tous droits réservés.
