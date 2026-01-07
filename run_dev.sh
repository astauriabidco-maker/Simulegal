#!/bin/bash

# Définir le répertoire du script comme répertoire de travail
cd "$(dirname "$0")"

echo "🚀 Démarrage du mode développement Simulegal..."

# Vérifier si python3 est installé
if ! command -v python3 &> /dev/null; then
    echo "❌ Python 3 n'est pas installé."
    exit 1
fi

# Créer un environnement virtuel s'il n'existe pas
if [ ! -d "venv" ]; then
    echo "📦 Création de l'environnement virtuel..."
    python3 -m venv venv
fi

# Activer l'environnement virtuel
source venv/bin/activate

# Installer les dépendances
echo "⬇️ Installation des dépendances..."
pip install -r requirements.txt

# Définir la base de données locale (SQLite)
export DATABASE_URL="sqlite+aiosqlite:///./simulegal.db"
export JWT_SECRET="dev-secret-key"

# Lancer le serveur
echo "✅ Serveur prêt ! Démarrage sur http://localhost:8001"
python3 -m uvicorn api.main:app --reload --host 0.0.0.0 --port 8001
