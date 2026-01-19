import { Prospect } from './SalesStore';

export const LeadScoring = {
    /**
     * Calcule le score d'un prospect en fonction de ses attributs et historique
     */
    calculateScore: (prospect: Prospect): number => {
        let score = 0;

        // 1. Source (Intentionnalité)
        switch (prospect.source) {
            case 'GOOGLE_ADS':
                score += 30; // Recherche active (Intent fort)
                break;
            case 'META_ADS':
                score += 20; // Push (Intent moyen)
                break;
            case 'TIKTOK_ADS':
                score += 10; // Découverte (Intent faible)
                break;
            case 'MANUAL':
                score += 15;
                break;
            default:
                score += 5;
        }

        // 2. Qualité du Contact
        if (prospect.email && prospect.phone) {
            score += 10;
        }
        if (prospect.email && !prospect.email.endsWith('@gmail.com') && !prospect.email.endsWith('@hotmail.com') && !prospect.email.endsWith('@yahoo.com')) {
            score += 5; // Email potentiellement pro ou moins "poubelle"
        }

        // 3. Activité / Comportement (Simulé)
        // Si le prospect a un service d'intérêt défini
        if (prospect.interestServiceId) {
            score += 10;
        }

        // Règles négatives
        if (prospect.status === 'LOST') {
            score = 0;
        }

        // Cap à 100
        return Math.min(100, Math.max(0, score));
    }
};
