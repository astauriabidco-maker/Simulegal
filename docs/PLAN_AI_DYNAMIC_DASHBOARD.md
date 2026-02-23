# Plan d'Implémentation : Dashboard Dynamique IA (Agent BI)

## La Vision
Remplacer les tableaux de bord statiques codés en dur par un système de **Business Intelligence Agentique**. L'Administrateur Siège interagit en langage naturel avec le système pour générer ses propres métriques, graphiques et rapports. Le tableau de bord devient une grille personnalisable ("Canvas") de widgets générés par l'IA.

## Architecture du Système

Le système repose sur 3 piliers :
1. **L'Interface Générative (Front-End)** : Un "Prompt Bar" sur le dashboard où l'admin tape sa requête.
2. **L'Agent d'Analyse (Back-End)** : Un service NestJS qui traduit le langage naturel en requêtes de base de données structurées.
3. **Le Moteur de Rendu Dynamique** : Un interpréteur React qui lit le JSON renvoyé par l'Agent et dessine le bon composant (Graphique, Carte de métrique, Tableau).

---

## Phase 1 : Le Moteur d'Interrogation (Text-to-Query)
**Objectif : Permettre à l'IA d'interroger la base de données SimuLegal de manière sécurisée.**

1. **Création du `AiReportingModule` (Backend)** :
   - Service qui utilise un LLM.
   - On lui fournit le schéma Prisma de SimuLegal (Modèles `Lead`, `Dossier`, `Transaction`, `User`).
   - L'Agent traduit la requête de l'admin (ex: *"CA de l'agence Paris en 2026"*) en une requête Prisma sécurisée ou un aggrégat MongoDB.
2. **Sécurité & Sandboxing** :
   - L'Agent n'a des droits qu'en LECTURE SEULE (`READONLY`).
   - Il applique automatiquement le scope de l'utilisateur (un Manager d'agence ne pourra générer des graphiques que sur son agence, même s'il le demande à l'IA).

## Phase 2 : Le Générateur de Widgets UI
**Objectif : Traduire les données brutes de l'IA en composants visuels.**

1. L'Agent IA, après exécution de la requête, répond au frontend avec un format JSON strict standardisé. Exemple :
   ```json
   {
      "type": "BAR_CHART",
      "title": "Chiffre d'affaires par mois",
      "data": [{"label": "Janvier", "value": 15000}, {"label": "Février", "value": 22000}],
      "insight": "Croissance de 46% par rapport au mois précédent."
   }
   ```
2. **Création du composant `<DynamicWidget />` (Frontend)** :
   - Ce composant lit le JSON et utilise une librairie comme `Recharts` ou `Chart.js` pour dessiner dynamiquement l'UI.
   - Ajout d'un bouton **"Épingler au Dashboard"** pour que l'admin sauvegarde ce widget personnalisé sur sa page d'accueil.

## Phase 3 : Mode Proactif (Insights Push)
**Objectif : L'Agent ne fait pas que répondre, il alerte.**

1. **CRON Job d'Analyse Nocturne** :
   - Chaque nuit, l'Agent BI scanne l'ensemble de l'activité.
   - Il cherche des anomalies : *"Baisse drastique des conversions sur le titre de séjour VPF"*, ou *"Goulot d'étranglement : 40 dossiers en attente de traitement chez Maître Dupont"*.
2. **Le "Newsfeed" du Dashboard** :
   - Au lieu de graphiques statiques, l'écran d'accueil affiche un flux d'intelligence (façon fil d'actualité) : "💡 *Insight du jour : Vos agences du sud performent 20% mieux cette semaine.*"

## Plan d'Action Technique Immédiat

- [ ] **Étape 1** : Créer l'endpoint Backend `/api/ai-reporting/query` qui prend un prompt en entrée.
- [ ] **Étape 2** : Écrire le prompt système (System Prompt) de l'Agent BI en lui injectant une version simplifiée du schéma Prisma.
- [ ] **Étape 3** : Modifier `DashboardLayout` et les pages d'accueil (`/admin`) pour inclure un "Chat/Prompt Agent" en haut de l'écran. 
- [ ] **Étape 4** : Implémenter le composant de grille dynamique (React Grid Layout) pour héberger les widgets générés pour chaque profil admin.
