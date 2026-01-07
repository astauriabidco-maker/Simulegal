# Simulegal

Simulegal est une application d'analyse d'éligibilité aux titres de séjour français.

## Installation et Démarrage Local

Pour faciliter le développement sans Docker, un script `run_dev.sh` est fourni.

### Prérequis
- Python 3.9+
- `pip`

### Démarrage Rapide

1. Exécutez le script de démarrage :
   ```bash
   ./run_dev.sh
   ```
   Ce script va :
   - Créer un environnement virtuel (`venv`) si nécessaire.
   - Installer les dépendances (`requirements.txt`).
   - Configurer les variables d'environnement locales.
   - Lancer le serveur backend sur `http://localhost:8001`.

2. Ouvrez votre navigateur sur [http://localhost:8001/frontend/index.html](http://localhost:8001/frontend/index.html).

### Architecture

- **Backend**: FastAPI (`api/`, `core/`)
- **Frontend**: HTML/JS/CSS + Alpine.js (`frontend/`)
- **Base de données**: SQLite (`simulegal.db`) en local, PostgreSQL en production.

### Développement Frontend

Le frontend a été migré vers **Alpine.js** pour une meilleure gestion de l'état.
- Les fichiers principaux sont dans `frontend/`.
- `index.html` contient la logique du questionnaire.

### Docker (Optionnel)

Pour lancer avec Docker (production ou environnement complet) :
```bash
docker-compose up --build
```
Note : Le port par défaut Docker peut être 8000. Le script local utilise 8001 pour éviter les conflits.
