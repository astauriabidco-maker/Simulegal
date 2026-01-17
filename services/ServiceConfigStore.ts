/**
 * Service de Configuration des Documents par Service
 * Permet de gérer dynamiquement les listes de documents requis sans toucher au code
 */

import { DOC_CATALOG, SERVICE_TEMPLATES as DEFAULT_TEMPLATES, DocumentRequirement } from '../config/DocumentTemplates';

const CONFIG_KEY = 'service_templates_config';

// Liste des services disponibles avec leurs métadonnées
export interface ServiceMetadata {
    id: string;
    name: string;
    description: string;
    category: 'IMMIGRATION' | 'DRIVING' | 'CIVIL' | 'OTHER';
    parentId?: string; // Pour l'héritage futur
}

export const AVAILABLE_SERVICES: ServiceMetadata[] = [
    // ========== NATURALISATION ==========
    {
        id: 'naturalisation',
        name: '🇫🇷 Naturalisation (Base)',
        description: 'Demande de nationalité française - cas général',
        category: 'IMMIGRATION'
    },
    {
        id: 'nat_declaration_mariage',
        name: '💍 Naturalisation par Mariage',
        description: 'Après 4 ans de mariage avec un conjoint français',
        category: 'IMMIGRATION',
        parentId: 'naturalisation'
    },
    {
        id: 'nat_droit_du_sol_18ans',
        name: '🎂 Naturalisation Droit du Sol',
        description: 'Pour les jeunes nés en France de parents étrangers',
        category: 'IMMIGRATION',
        parentId: 'naturalisation'
    },
    {
        id: 'nat_decret_etudes_sup',
        name: '🎓 Naturalisation Études Supérieures',
        description: 'Parcours facilité avec diplôme français',
        category: 'IMMIGRATION',
        parentId: 'naturalisation'
    },
    {
        id: 'nat_decret_refugie',
        name: '🛡️ Naturalisation Réfugié',
        description: 'Pour les réfugiés et apatrides',
        category: 'IMMIGRATION',
        parentId: 'naturalisation'
    },

    // ========== CARTES DE RÉSIDENT ==========
    {
        id: 'carte_resident_longue_duree_ue',
        name: '🏠 Carte Résident Longue Durée UE',
        description: 'Après 5 ans de résidence légale',
        category: 'IMMIGRATION'
    },
    {
        id: 'carte_resident_conjoint_francais',
        name: '💑 Carte Résident Conjoint Français',
        description: 'Après 3 ans de mariage',
        category: 'IMMIGRATION'
    },
    {
        id: 'carte_resident_regroupement_familial',
        name: '👨‍👩‍👧 Carte Résident Regroupement Familial',
        description: 'Pour les personnes entrées par regroupement',
        category: 'IMMIGRATION'
    },

    // ========== TITRES VPF ==========
    {
        id: 'titre_sejour',
        name: '📄 Titre de Séjour (Base)',
        description: 'Documents communs à tous les titres',
        category: 'IMMIGRATION'
    },
    {
        id: 'vpf_conjoint_francais',
        name: '💍 VPF - Conjoint de Français',
        description: 'Carte Vie Privée et Familiale',
        category: 'IMMIGRATION',
        parentId: 'titre_sejour'
    },
    {
        id: 'vpf_parent_enfant_francais',
        name: '👶 VPF - Parent Enfant Français',
        description: 'Parent contribuant à l\'éducation',
        category: 'IMMIGRATION',
        parentId: 'titre_sejour'
    },
    {
        id: 'vpf_pacs_francais',
        name: '📝 VPF - PACS avec Français',
        description: 'Partenaire pacsé avec vie commune',
        category: 'IMMIGRATION',
        parentId: 'titre_sejour'
    },
    {
        id: 'vpf_humanitaire_violence',
        name: '🛡️ VPF - Protection Violence',
        description: 'Victime de violences conjugales',
        category: 'IMMIGRATION',
        parentId: 'titre_sejour'
    },

    // ========== TITRES TRAVAIL ==========
    {
        id: 'cs_salarie',
        name: '💼 Carte Séjour - Salarié CDI',
        description: 'Travailleur avec contrat CDI',
        category: 'IMMIGRATION'
    },
    {
        id: 'cs_travailleur_temporaire',
        name: '⏱️ Carte Séjour - Travailleur CDD',
        description: 'Travailleur avec contrat temporaire',
        category: 'IMMIGRATION'
    },
    {
        id: 'cs_entrepreneur_liberale',
        name: '🏢 Carte Séjour - Entrepreneur',
        description: 'Création d\'activité ou profession libérale',
        category: 'IMMIGRATION'
    },

    // ========== TITRES ÉTUDIANTS ==========
    {
        id: 'cs_etudiant',
        name: '🎓 Carte Séjour - Étudiant',
        description: 'Inscription dans le supérieur',
        category: 'IMMIGRATION'
    },
    {
        id: 'rece_post_master',
        name: '🔍 Carte RECE Post-Master',
        description: 'Recherche d\'emploi après diplôme',
        category: 'IMMIGRATION'
    },

    // ========== PASSEPORT TALENT ==========
    {
        id: 'passeport_talent_carte_bleue_eu',
        name: '🔵 Passeport Talent - Carte Bleue UE',
        description: 'Travailleur hautement qualifié',
        category: 'IMMIGRATION'
    },
    {
        id: 'passeport_talent_salarie_qualifie',
        name: '⭐ Passeport Talent - Salarié Qualifié',
        description: 'Diplômé Master avec salaire > 43k€',
        category: 'IMMIGRATION'
    },
    {
        id: 'passeport_talent_investisseur',
        name: '💰 Passeport Talent - Investisseur',
        description: 'Investissement direct > 300k€',
        category: 'IMMIGRATION'
    },
    {
        id: 'passeport_talent_creation',
        name: '🚀 Passeport Talent - Création Entreprise',
        description: 'Projet de création avec 30k€',
        category: 'IMMIGRATION'
    },
    {
        id: 'passeport_talent_entreprise_innovante',
        name: '💡 Passeport Talent - Entreprise Innovante',
        description: 'Recrutement par JEI pour R&D',
        category: 'IMMIGRATION'
    },
    {
        id: 'passeport_talent_mandataire',
        name: '👔 Passeport Talent - Mandataire Social',
        description: 'Représentant légal d\'entreprise',
        category: 'IMMIGRATION'
    },

    // ========== AUTRES IMMIGRATION ==========
    {
        id: 'nat_declaration_fratrie',
        name: '👫 Naturalisation par Fratrie',
        description: 'Pour les frères/sœurs de français nés en France',
        category: 'IMMIGRATION'
    },
    {
        id: 'nat_declaration_ascendant',
        name: '👴 Naturalisation par Ascendant',
        description: 'Pour les parents de français âgés de plus de 65 ans',
        category: 'IMMIGRATION'
    },
    {
        id: 'carte_resident_refugie_apatride',
        name: '🛡️ Carte Résident Réfugié/Apatride',
        description: 'Protection internationale accordée par l\'OFPRA',
        category: 'IMMIGRATION'
    },
    {
        id: 'passeport_talent_famille',
        name: '👨‍👩‍👧 Passeport Talent Famille',
        description: 'Accompagnants de titulaire Passeport Talent',
        category: 'IMMIGRATION'
    },
    {
        id: 'cs_saisonnier',
        name: '🍎 Carte Séjour - Saisonnier',
        description: 'Travail saisonnier (6 mois/an max)',
        category: 'IMMIGRATION'
    },
    {
        id: 'aps_enfant_malade',
        name: '🏥 APS - Enfant Malade',
        description: 'Séjour pour soins médicaux d\'un enfant',
        category: 'IMMIGRATION'
    },
    // ========== CERTIFICAT RÉSIDENCE ALGÉRIEN (CRA) ==========
    {
        id: 'cra_algerien_resident_10ans',
        name: '🇩🇿 Algérien - Résident 10 ans',
        description: 'Certificat de résidence pour ressortissants algériens',
        category: 'IMMIGRATION'
    },
    {
        id: 'cra_algerien_conjoint_francais',
        name: '🇩🇿 Algérien - Conjoint Français',
        description: 'VPF spécifique accord franco-algérien',
        category: 'IMMIGRATION'
    },
    {
        id: 'cra_algerien_activite_liberale',
        name: '🇩🇿 Algérien - Profession Libérale',
        description: 'Création d\'activité pour ressortissants algériens',
        category: 'IMMIGRATION'
    },

    // ========== CITOYENS EUROPÉENS ==========
    {
        id: 'cs_citoyen_ue_inactif_ou_actif',
        name: '🇪🇺 Citoyen UE - Séjour',
        description: 'Attestation d\'enregistrement citoyen UE',
        category: 'IMMIGRATION'
    },
    {
        id: 'cs_membre_famille_ue',
        name: '🇪🇺 Membre Famille UE',
        description: 'Carte de séjour membre de famille d\'un citoyen UE',
        category: 'IMMIGRATION'
    },

    // ========== ADMISSION EXCEPTIONNELLE (AES) ==========
    {
        id: 'aes_metiers_tension',
        name: '⚡ AES - Métiers en tension',
        description: 'Régularisation par le travail (circulaire Valls)',
        category: 'IMMIGRATION'
    },

    // ========== NATURALISATION - AUTRES ==========
    {
        id: 'nat_droit_du_sol_anticipe_13_16',
        name: '🎂 Nat. Droit du Sol (Anticipée)',
        description: 'Pour les 13-16 ans nés en France',
        category: 'IMMIGRATION'
    },
    {
        id: 'nat_decret_standard',
        name: '🇫🇷 Naturalisation par Décret',
        description: 'Procédure standard de naturalisation',
        category: 'IMMIGRATION'
    },

    {
        id: 'default',
        name: '⚙️ Configuration par Défaut',
        description: 'Documents demandés si le service n\'est pas configuré',
        category: 'OTHER'
    }
];



// Clés de stockage
const CUSTOM_DOCS_KEY = 'custom_documents';
const CUSTOM_SERVICES_KEY = 'custom_services';

export const ServiceConfigStore = {
    /**
     * Charge toutes les configurations (localStorage ou par défaut)
     */
    getAllConfigs: (): Record<string, string[]> => {
        if (typeof window === 'undefined') return DEFAULT_TEMPLATES;

        const saved = localStorage.getItem(CONFIG_KEY);
        if (saved) {
            try {
                return JSON.parse(saved);
            } catch {
                console.warn('[CONFIG] Erreur de parsing, utilisation des valeurs par défaut');
                return DEFAULT_TEMPLATES;
            }
        }
        return DEFAULT_TEMPLATES;
    },

    /**
     * Récupère la configuration pour un service spécifique
     */
    getServiceConfig: (serviceId: string): string[] => {
        const configs = ServiceConfigStore.getAllConfigs();
        return configs[serviceId] || configs['default'] || [];
    },

    /**
     * Sauvegarde une modification pour un service donné
     */
    updateServiceConfig: (serviceId: string, docIds: string[]): void => {
        const current = ServiceConfigStore.getAllConfigs();
        current[serviceId] = docIds;
        localStorage.setItem(CONFIG_KEY, JSON.stringify(current));
        console.log(`[CONFIG] ✅ Mise à jour des documents pour "${serviceId}" (${docIds.length} documents)`);
    },

    /**
     * Récupère les objets documents complets pour un service
     * C'est cette fonction qui doit être utilisée par le Checkout
     */
    getRequirements: (serviceId: string): DocumentRequirement[] => {
        const configs = ServiceConfigStore.getAllConfigs();
        const normalizedId = serviceId.toLowerCase().replace(/[^a-z_]/g, '_');

        // Fallback sur 'default' si le service n'est pas configuré
        const docIds = configs[normalizedId] || configs['default'] || [];

        // Récupère tous les documents (catalogue + custom)
        const allDocs = ServiceConfigStore.getAllDocuments();
        const docMap = new Map(allDocs.map(d => [d.id, d]));

        // Transforme les IDs en objets complets
        return docIds
            .map(id => docMap.get(id))
            .filter((doc): doc is DocumentRequirement => doc !== undefined);
    },

    /**
     * Réinitialise un service à sa configuration par défaut
     */
    resetServiceConfig: (serviceId: string): void => {
        const current = ServiceConfigStore.getAllConfigs();
        const defaultConfig = DEFAULT_TEMPLATES[serviceId];

        if (defaultConfig) {
            current[serviceId] = [...defaultConfig];
            localStorage.setItem(CONFIG_KEY, JSON.stringify(current));
            console.log(`[CONFIG] 🔄 Réinitialisation de "${serviceId}" aux valeurs par défaut`);
        }
    },

    /**
     * Réinitialise toutes les configurations aux valeurs par défaut
     */
    resetAllConfigs: (): void => {
        localStorage.removeItem(CONFIG_KEY);
        console.log('[CONFIG] 🔄 Toutes les configurations réinitialisées');
    },

    // ============================================
    // GESTION DES DOCUMENTS (CRUD)
    // ============================================

    /**
     * Récupère tous les documents (catalogue par défaut + personnalisés)
     */
    getAllDocuments: (): DocumentRequirement[] => {
        const customDocs = ServiceConfigStore.getCustomDocuments();
        return [...Object.values(DOC_CATALOG), ...customDocs];
    },

    /**
     * Récupère les documents personnalisés
     */
    getCustomDocuments: (): DocumentRequirement[] => {
        if (typeof window === 'undefined') return [];
        const saved = localStorage.getItem(CUSTOM_DOCS_KEY);
        if (saved) {
            try {
                return JSON.parse(saved);
            } catch {
                return [];
            }
        }
        return [];
    },

    /**
     * Ajoute un nouveau document au catalogue personnalisé
     */
    addDocument: (doc: DocumentRequirement): boolean => {
        // Vérifie que l'ID n'existe pas déjà
        const allDocs = ServiceConfigStore.getAllDocuments();
        if (allDocs.some(d => d.id === doc.id)) {
            console.error(`[DOCS] ❌ Document "${doc.id}" existe déjà`);
            return false;
        }

        const customDocs = ServiceConfigStore.getCustomDocuments();
        customDocs.push(doc);
        localStorage.setItem(CUSTOM_DOCS_KEY, JSON.stringify(customDocs));
        console.log(`[DOCS] ✅ Document "${doc.id}" ajouté`);
        return true;
    },

    /**
     * Met à jour un document personnalisé
     */
    updateDocument: (docId: string, updates: Partial<DocumentRequirement>): boolean => {
        const customDocs = ServiceConfigStore.getCustomDocuments();
        const index = customDocs.findIndex(d => d.id === docId);

        if (index === -1) {
            // Vérifie si c'est un document par défaut (non modifiable)
            if (DOC_CATALOG[docId]) {
                console.error(`[DOCS] ❌ Impossible de modifier un document par défaut`);
                return false;
            }
            console.error(`[DOCS] ❌ Document "${docId}" non trouvé`);
            return false;
        }

        customDocs[index] = { ...customDocs[index], ...updates, id: docId };
        localStorage.setItem(CUSTOM_DOCS_KEY, JSON.stringify(customDocs));
        console.log(`[DOCS] ✅ Document "${docId}" mis à jour`);
        return true;
    },

    /**
     * Supprime un document personnalisé
     */
    deleteDocument: (docId: string): boolean => {
        // Vérifie si c'est un document par défaut (non supprimable)
        if (DOC_CATALOG[docId]) {
            console.error(`[DOCS] ❌ Impossible de supprimer un document par défaut`);
            return false;
        }

        const customDocs = ServiceConfigStore.getCustomDocuments();
        const filtered = customDocs.filter(d => d.id !== docId);

        if (filtered.length === customDocs.length) {
            console.error(`[DOCS] ❌ Document "${docId}" non trouvé`);
            return false;
        }

        localStorage.setItem(CUSTOM_DOCS_KEY, JSON.stringify(filtered));
        console.log(`[DOCS] ✅ Document "${docId}" supprimé`);
        return true;
    },

    /**
     * Vérifie si un document est personnalisé (modifiable/supprimable)
     */
    isCustomDocument: (docId: string): boolean => {
        return !DOC_CATALOG[docId];
    },

    /**
     * Récupère les documents par catégorie
     */
    getDocumentsByCategory: (category: string): DocumentRequirement[] => {
        return ServiceConfigStore.getAllDocuments().filter(doc => doc.category === category);
    },

    // ============================================
    // GESTION DES SERVICES (CRUD)
    // ============================================

    /**
     * Récupère tous les services (par défaut + personnalisés)
     */
    getAllServices: (): ServiceMetadata[] => {
        const customServices = ServiceConfigStore.getCustomServices();
        return [...AVAILABLE_SERVICES, ...customServices];
    },

    /**
     * Récupère les services personnalisés
     */
    getCustomServices: (): ServiceMetadata[] => {
        if (typeof window === 'undefined') return [];
        const saved = localStorage.getItem(CUSTOM_SERVICES_KEY);
        if (saved) {
            try {
                return JSON.parse(saved);
            } catch {
                return [];
            }
        }
        return [];
    },

    /**
     * Ajoute un nouveau service
     */
    addService: (service: ServiceMetadata, initialDocIds: string[] = []): boolean => {
        // Vérifie que l'ID n'existe pas déjà
        const allServices = ServiceConfigStore.getAllServices();
        if (allServices.some(s => s.id === service.id)) {
            console.error(`[SERVICES] ❌ Service "${service.id}" existe déjà`);
            return false;
        }

        const customServices = ServiceConfigStore.getCustomServices();
        customServices.push(service);
        localStorage.setItem(CUSTOM_SERVICES_KEY, JSON.stringify(customServices));

        // Initialise la configuration du service avec les documents
        if (initialDocIds.length > 0) {
            ServiceConfigStore.updateServiceConfig(service.id, initialDocIds);
        }

        console.log(`[SERVICES] ✅ Service "${service.id}" ajouté`);
        return true;
    },

    /**
     * Met à jour un service personnalisé
     */
    updateService: (serviceId: string, updates: Partial<ServiceMetadata>): boolean => {
        const customServices = ServiceConfigStore.getCustomServices();
        const index = customServices.findIndex(s => s.id === serviceId);

        if (index === -1) {
            // Vérifie si c'est un service par défaut (non modifiable)
            if (AVAILABLE_SERVICES.some(s => s.id === serviceId)) {
                console.error(`[SERVICES] ❌ Impossible de modifier un service par défaut`);
                return false;
            }
            console.error(`[SERVICES] ❌ Service "${serviceId}" non trouvé`);
            return false;
        }

        customServices[index] = { ...customServices[index], ...updates, id: serviceId };
        localStorage.setItem(CUSTOM_SERVICES_KEY, JSON.stringify(customServices));
        console.log(`[SERVICES] ✅ Service "${serviceId}" mis à jour`);
        return true;
    },

    /**
     * Supprime un service personnalisé
     */
    deleteService: (serviceId: string): boolean => {
        // Vérifie si c'est un service par défaut (non supprimable)
        if (AVAILABLE_SERVICES.some(s => s.id === serviceId)) {
            console.error(`[SERVICES] ❌ Impossible de supprimer un service par défaut`);
            return false;
        }

        const customServices = ServiceConfigStore.getCustomServices();
        const filtered = customServices.filter(s => s.id !== serviceId);

        if (filtered.length === customServices.length) {
            console.error(`[SERVICES] ❌ Service "${serviceId}" non trouvé`);
            return false;
        }

        localStorage.setItem(CUSTOM_SERVICES_KEY, JSON.stringify(filtered));

        // Supprime aussi la configuration associée
        const configs = ServiceConfigStore.getAllConfigs();
        delete configs[serviceId];
        localStorage.setItem(CONFIG_KEY, JSON.stringify(configs));

        console.log(`[SERVICES] ✅ Service "${serviceId}" supprimé`);
        return true;
    },

    /**
     * Vérifie si un service est personnalisé (modifiable/supprimable)
     */
    isCustomService: (serviceId: string): boolean => {
        return !AVAILABLE_SERVICES.some(s => s.id === serviceId);
    },

    /**
     * Vérifie si une configuration a été modifiée par rapport aux valeurs par défaut
     */
    isModified: (serviceId: string): boolean => {
        const current = ServiceConfigStore.getServiceConfig(serviceId);
        const defaultConfig = DEFAULT_TEMPLATES[serviceId] || [];

        if (current.length !== defaultConfig.length) return true;
        return !current.every((id, idx) => id === defaultConfig[idx]);
    },

    /**
     * Obtient les métadonnées d'un service
     */
    getServiceMetadata: (serviceId: string): ServiceMetadata | undefined => {
        return ServiceConfigStore.getAllServices().find(s => s.id === serviceId);
    }
};

export default ServiceConfigStore;

