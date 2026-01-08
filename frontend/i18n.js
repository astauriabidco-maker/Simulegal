const translations = {
    fr: {
        app_title: "Simulegal",
        tagline: "Évaluez votre éligibilité aux titres de séjour en France",
        nav_login: "Se connecter",
        nav_logout: "Déconnexion",
        nav_admin: "Administration",
        nav_dashboard: "Mon Espace",
        nav_hello: "Bonjour",
        nav_dashboard: "Mon Espace",
        nav_hello: "Bonjour",
        nav_simulateur: "Simulateur",
        nav_how_it_works: "Comment ça marche",
        nav_features: "Avantages",
        nav_commencer: "Commencer",
        btn_chat: "Discuter avec notre IA",
        // --- LANDING PAGE ---
        hero_title: "Votre titre de séjour, <br><span class='gradient-text'>analysé par IA.</span>",
        hero_subtitle: "Obtenez une évaluation précise de votre éligibilité en moins de 3 minutes. Basé sur les 115 critères officiels du CESEDA.",
        hero_cta: "Faire le test gratuit →",
        trust_text: "BASÉ SUR LES TEXTES OFFICIELS & SOURCES GOUVERNEMENTALES",
        feature_1_title: "Analyse Instantanée",
        feature_1_desc: "Ne perdez plus des mois à attendre un rendez-vous. Notre IA analyse votre dossier en temps réel.",
        feature_2_title: "100% Anonyme",
        feature_2_desc: "Vos données ne quittent pas votre navigateur avant soumission. Confidentialité totale garantie.",
        feature_3_title: "Checklist Précise",
        feature_3_desc: "Recevez la liste exacte des pièces justificatives à fournir selon votre situation spécifique.",
        how_title: "Comment ça marche ?",
        how_step_1_title: "Répondez",
        how_step_1_desc: "15 questions simples sur votre parcours.",
        how_step_2_title: "Analysez",
        how_step_2_desc: "Notre moteur croise 115 critères juridiques.",
        how_step_3_title: "Obtenez",
        how_step_3_desc: "Votre score d'éligibilité et démarches.",
        footer_desc: "L'intelligence juridique simplifiée pour votre avenir en France.",
        footer_nav: "Navigation",
        footer_legal: "Légal",
        footer_rights: "© 2026 SimuLegal. Tous droits réservés. Basé sur les textes du CESEDA.",

        // --- AUTH / LOGIN ---
        auth_tagline: "Espace Personnel & Administration",
        tab_login: "Connexion",
        tab_register: "Inscription",
        lbl_email: "Email",
        lbl_password: "Mot de passe",
        lbl_fullname: "Nom complet",
        btn_login: "Se connecter",
        btn_register: "S'inscrire",

        // --- DASHBOARD ---
        dash_title: "Mon Espace",
        dash_subtitle: "Gérez vos dossiers et suivez votre progression",
        dash_history: "📜 Historique des simulations",
        dash_empty: "Vous n'avez pas encore de simulation enregistrée.",
        dash_loading: "Chargement de vos dossiers...",
        dash_roadmap: "🗺️ Ma Roadmap de Régularisation",
        dash_roadmap_empty: "Réalisez une simulation pour voir votre parcours.",
        dash_tip_title: "💡 Conseil du jour",
        dash_tip_desc: "Pensez à bien conserver vos fiches de paie et justificatifs de domicile. Ces documents sont systématiquement demandés quel que soit le titre.",
        status_paid: "Payé",
        status_unpaid: "Non payé",
        btn_view: "Voir",
        btn_pdf: "PDF",

        // --- CHAT ---
        chat_title: "Simulegal AI",
        chat_subtitle: "Votre expert en droit des étrangers",
        chat_online: "Assistant en ligne",
        chat_welcome: "Bonjour ! Je suis l'assistant intelligent de Simulegal. Je vais vous aider à évaluer votre éligibilité en quelques questions.",
        chat_q1: "Pour commencer, quelle est votre nationalité ?",
        chat_placeholder: "Type your message here...",
        chat_finished: "Analyse terminée ! Cliquez ci-dessous pour voir votre score d'éligibilité complet.",
        btn_see_results: "Voir mes résultats",
        badge_new: "✨ Nouveau : Moteur IA v2.0",
        stat_analyses: "analyses",
        chk_hosted: "Hébergement communautaire",
        loading_text: "Analyse en cours...",
        loading_done: "Analyse terminée !",
        nav_home: "Accueil",
        link_terms: "Conditions Générales",
        link_privacy: "Protection des Données",
        link_legal: "Mentions Légales",

        step_situation: "Situation",
        step_criteres: "Critères",
        step_resultats: "Résultats",
        btn_next: "Suivant",
        btn_next_disabled: "Veuillez répondre pour continuer",
        btn_prev: "Précédent",
        btn_evaluate: "Analyser mon éligibilité",
        locked_title: "🔒 Dossier Complet Verrouillé",
        locked_desc: "Pour accéder à la liste précise des documents et télécharger votre rapport officiel PDF, débloquez votre dossier maintenant.",
        btn_unlock: "Débloquer le Dossier (9.90€)",
        docs_required: "📄 Documents requis",
        score_high: "Éligible",
        score_medium: "Partiel",
        score_low: "Peu probable",
        lbl_nationality: "Nationalité",
        lbl_residence: "Durée de résidence en France (mois)",
        lbl_french: "Niveau de français (CECRL)",

        // --- NOUVEAUX ELEMENTS (CARTES) ---
        // Step 1.1: Objectif
        step1_title: "Quel est votre objectif principal ?",
        step1_subtitle: "Choisissez la démarche que vous souhaitez entreprendre.",
        goal_nat_title: "Devenir Français",
        goal_nat_desc: "Naturalisation, mariage, naissance...",
        goal_res_title: "Titre de Séjour",
        goal_res_desc: "Carte de séjour, résidence, visa...",
        goal_both_title: "Explorer les options",
        goal_both_desc: "Je ne sais pas encore / Les deux",

        // Step 1.2: Statut Actuel
        step1_2_title: "Votre statut actuel",
        step1_2_subtitle: "Sélectionnez le document ou la situation qui correspond à votre cas.",
        status_none_title: "Sans Papiers",
        status_none_desc: "Je n'ai pas de titre de séjour valide actuellement.",
        status_vls_title: "Visa Long Séjour (VLS-TS)",
        status_vls_desc: "Visa validé comme titre de séjour.",
        status_card_title: "Carte de Séjour (1-4 ans)",
        status_card_desc: "Temporaire ou Pluriannuelle.",
        status_res_title: "Carte de 10 ans",
        status_res_desc: "Résident de longue durée.",
        status_recep_title: "Récépissé",
        status_recep_desc: "Demande ou renouvellement en cours.",
        status_other_title: "Autre Situation",
        status_other_desc: "Étudiant, Talent, Touriste, Européen...",

        // Step 2.1: Famille
        step2_1_title: "Votre situation familiale",
        step2_1_subtitle: "Information cruciale pour le regroupement familial.",
        fam_single_title: "Célibataire",
        fam_single_desc: "Non marié(e), non pacsé(e).",
        fam_married_title: "Marié(e)",
        fam_married_desc: "Mariage civil reconnu.",
        fam_pacs_title: "Pacsé(e) / Concubinage",
        fam_pacs_desc: "Partenariat civil ou union libre.",
        fam_div_title: "Divorcé(e) ou Veuf(ve)",
        fam_div_desc: "Anciennement marié(e).",

        // Step 1.3: Nationalité
        step1_3_title: "Votre Nationalité",
        lbl_nationality_q: "De quel pays êtes-vous ressortissant ?",
        opt_select_nat: "Sélectionnez votre nationalité...",
        opt_americas: "Amériques",
        opt_maghreb: "Afrique du Nord",
        opt_west_africa: "Afrique de l'Ouest",
        opt_asia: "Asie",
        opt_other: "Autre",

        // Step 1.4: Parcours
        step1_4_title: "Votre Parcours en France",
        lbl_residence_duration: "Durée totale de présence",
        lbl_french_level: "Niveau de français",
        opt_no_level_fr: "Aucun qualification (A0)",
        opt_year: "an",
        opt_years: "ans",
        opt_month: "mois",

        // Step 1.5: Revenus
        step1_5_title: "Vos Revenus",
        lbl_salary: "Revenu mensuel net moyen (€)",
        hint_salary: "Indispensable pour calculer l'autonomie financière.",

        // Step 2.2: Liens France
        step2_2_title: "Vos Liens avec la France",
        lbl_personal_path: "Parcours Personnel & Éducation",
        chk_born_france: "Je suis né(e) en France de parents étrangers",
        chk_schooling: "J'ai effectué ma scolarité en France (6 à 16 ans)",
        chk_res_18: "Je résidais en France à mes 18 ans",
        chk_entered_minor: "Entré(e) mineur(e) par regroupement familial",
        lbl_family_links: "Famille & Liens en France",
        chk_sibling_fr: "J'ai un frère/sœur français(e)",
        chk_ascendant_fr: "Je suis parent/grand-parent d'un Français",
        chk_adopted_fr: "Adopté(e) par un Français",
        chk_recueilli_fr: "Recueilli(e) mineur(e) par un Français",
        lbl_res_with_fr: "Années de résidence avec ce citoyen français",

        // Step 2.3: Education & Travail
        step2_3_title: "Éducation & Travail",
        lbl_education: "Niveau d'études le plus élevé",
        opt_edu_none: "Aucun diplôme supérieur",
        opt_edu_bac3: "Licence / Bachelor (Bac+3)",
        opt_edu_master_fr: "Master obtenu en France (Bac+5)",
        opt_edu_master_foreign: "Master obtenu à l'étranger",
        opt_edu_phd: "Doctorat",
        lbl_job: "Situation professionnelle",
        opt_job_none: "Sans emploi / Etudiant",
        opt_job_cdi: "CDI",
        opt_job_cdd: "CDD",
        opt_job_interim: "Intérim",
        opt_job_freelance: "Indépendant",

        // Step 2.4: Expert
        step2_4_title: "Cas Particuliers",
        chk_health: "Problème de santé grave (soins indispensables)",
        chk_refugee: "Reconnu Réfugié / Apatride",
        chk_no_oqtf: "Je n'ai pas d'OQTF non respectée",
        btn_expert: "🛠️ Activer les Options Avancées",
        txt_expert_intro: "Le système expert analysera plus de 50 critères additionnels.",
        cat_military: "🎖️ Services & Militaire",
        chk_veteran: "Ancien combattant",
        chk_combatant_card: "Carte du combattant",
        chk_exc_services: "Services exceptionnels",
        cat_health: "🏥 Santé & Social",
        chk_pension: "Rente AT/MP",
        chk_child_care: "Enfant nécessitant soins",

        // Navigation Buttons
        btn_new_sim: "← Nouvelle simulation",
        btn_submit: "Analyser mon dossier",

        // Other Step 2 family details
        lbl_marriage_years: "Durée du mariage (années)",
        lbl_spouse_nat: "Nationalité du conjoint",
        opt_spouse_foreign: "Etrangère (Hors UE)",
        opt_spouse_fr: "Française",
        opt_spouse_eu: "Union Européenne",
        chk_parent_fr_child: "Je suis parent d'un enfant français"
    },
    en: {
        app_title: "Simulegal",
        tagline: "Assess your eligibility for French residence permits",
        nav_login: "Login",
        nav_logout: "Logout",
        nav_admin: "Admin",
        nav_dashboard: "My Dashboard",
        nav_hello: "Hello",
        nav_dashboard: "My Dashboard",
        nav_hello: "Hello",
        nav_simulateur: "Simulator",
        nav_how_it_works: "How it works",
        nav_features: "Features",
        nav_commencer: "Start",
        btn_chat: "Chat with our IA",
        // --- LANDING PAGE ---
        hero_title: "Your residence permit, <br><span class='gradient-text'>analyzed by AI.</span>",
        hero_subtitle: "Get a precise assessment of your eligibility in less than 3 minutes. Based on the 115 official CESEDA criteria.",
        hero_cta: "Take the free test →",
        trust_text: "BASED ON OFFICIAL TEXTS & GOVERNMENT SOURCES",
        feature_1_title: "Instant Analysis",
        feature_1_desc: "Don't waste months waiting for an appointment. Our AI analyzes your file in real time.",
        feature_2_title: "100% Anonymous",
        feature_2_desc: "Your data does not leave your browser before submission. Total privacy guaranteed.",
        feature_3_title: "Precise Checklist",
        feature_3_desc: "Receive the exact list of supporting documents to provide according to your specific situation.",
        how_title: "How it works?",
        how_step_1_title: "Answer",
        how_step_1_desc: "15 simple questions about your journey.",
        how_step_2_title: "Analyze",
        how_step_2_desc: "Our engine cross-references 115 legal criteria.",
        how_step_3_title: "Get",
        how_step_3_desc: "Your eligibility score and next steps.",
        footer_desc: "Simplified legal intelligence for your future in France.",
        footer_nav: "Navigation",
        footer_legal: "Legal",
        footer_rights: "© 2026 SimuLegal. All rights reserved. Based on CESEDA texts.",

        // --- AUTH / LOGIN ---
        auth_tagline: "Personal Space & Administration",
        tab_login: "Login",
        tab_register: "Registration",
        lbl_email: "Email",
        lbl_password: "Password",
        lbl_fullname: "Full Name",
        btn_login: "Login",
        btn_register: "Register",

        // --- DASHBOARD ---
        dash_title: "My Dashboard",
        dash_subtitle: "Manage your files and track your progress",
        dash_history: "📜 Simulation History",
        dash_empty: "You don't have any saved simulations yet.",
        dash_loading: "Loading your files...",
        dash_roadmap: "🗺️ My Regularization Roadmap",
        dash_roadmap_empty: "Run a simulation to see your path.",
        dash_tip_title: "💡 Tip of the day",
        dash_tip_desc: "Remember to keep your payslips and proof of address. These documents are systematically requested regardless of the permit.",
        status_paid: "Paid",
        status_unpaid: "Unpaid",
        btn_view: "View",
        btn_pdf: "PDF",

        // --- CHAT ---
        chat_title: "Simulegal AI",
        chat_subtitle: "Your expert in foreign law",
        chat_online: "Online Assistant",
        chat_welcome: "Hello! I am the intelligent assistant of Simulegal. I will help you assess your eligibility in a few questions.",
        chat_q1: "To start, what is your nationality?",
        chat_placeholder: "Type your message here...",
        chat_finished: "Analysis finished! Click below to see your full eligibility score.",
        btn_see_results: "See my results",
        badge_new: "✨ New: AI Engine v2.0",
        stat_analyses: "analyses",
        chk_hosted: "Community housing",
        loading_text: "Analysis in progress...",
        loading_done: "Analysis complete!",
        nav_home: "Home",
        link_terms: "Terms & Conditions",
        link_privacy: "Data Protection",
        link_legal: "Legal Notice",

        step_situation: "Situation",
        step_criteres: "Criteria",
        step_resultats: "Results",
        btn_next: "Next",
        btn_next_disabled: "Please answer to continue",
        btn_prev: "Previous",
        btn_evaluate: "Analyze my eligibility",
        locked_title: "🔒 Full Records Locked",
        locked_desc: "To access the precise list of documents and download your official PDF report, unlock your records now.",
        btn_unlock: "Unlock Full Records (9.90€)",
        docs_required: "📄 Required Documents",
        score_high: "Eligible",
        score_medium: "Partial",
        score_low: "Unlikely",
        lbl_nationality: "Nationality",
        lbl_residence: "Duration of residence (months)",
        lbl_french: "French level",

        // --- NEW ELEMENTS (CARDS) ---
        step1_title: "What is your main goal?",
        step1_subtitle: "Choose the process you wish to undertake.",
        goal_nat_title: "Become French",
        goal_nat_desc: "Naturalization, marriage, birth...",
        goal_res_title: "Residence Permit",
        goal_res_desc: "Residence card, visa, renewal...",
        goal_both_title: "Explore Options",
        goal_both_desc: "I don't know yet / Both",

        step1_2_title: "Your current status",
        step1_2_subtitle: "Select the document or situation that applies to you.",
        status_none_title: "Undocumented",
        status_none_desc: "I do not currently have a valid residence permit.",
        status_vls_title: "Long Stay Visa (VLS-TS)",
        status_vls_desc: "Visa validated as a residence permit.",
        status_card_title: "Residence Card (1-4 years)",
        status_card_desc: "Temporary or Multi-year card.",
        status_res_title: "10-Year Card",
        status_res_desc: "Long-term resident.",
        status_recep_title: "Receipt (Récépissé)",
        status_recep_desc: "Application or renewal in progress.",
        status_other_title: "Other Situation",
        status_other_desc: "Student, Talent, Tourist, European...",

        step2_1_title: "Your family situation",
        step2_1_subtitle: "Crucial for family reunification.",
        fam_single_title: "Single",
        fam_single_desc: "Not married, not in a civil partnership.",
        fam_married_title: "Married",
        fam_married_desc: "Recognized civil marriage.",
        fam_pacs_title: "Civil Partnership / Cohabitation",
        fam_pacs_desc: "PACS or free union.",
        fam_div_title: "Divorced or Widowed",
        fam_div_desc: "Formerly married.",

        // Step 1.3: Nationality
        step1_3_title: "Your Nationality",
        lbl_nationality_q: "Which country are you a national of?",
        opt_select_nat: "Select your nationality...",
        opt_americas: "Americas",
        opt_maghreb: "North Africa",
        opt_west_africa: "West Africa",
        opt_asia: "Asia",
        opt_other: "Other",

        // Step 1.4: Path
        step1_4_title: "Your Path in France",
        lbl_residence_duration: "Total duration of presence",
        lbl_french_level: "French Level",
        opt_no_level_fr: "No qualification (A0)",
        opt_year: "year",
        opt_years: "years",
        opt_month: "month",

        // Step 1.5: Income
        step1_5_title: "Your Income",
        lbl_salary: "Average monthly net income (€)",
        hint_salary: "Essential for calculating financial autonomy.",

        // Step 2.2: Links to France
        step2_2_title: "Your Links with France",
        lbl_personal_path: "Personal Path & Education",
        chk_born_france: "Born in France to foreign parents",
        chk_schooling: "Schooled in France (ages 6 to 16)",
        chk_res_18: "Habitual resident in France at age 18",
        chk_entered_minor: "Entered as minor via family reunification",
        lbl_family_links: "Family & Links in France",
        chk_sibling_fr: "I have a French brother/sister",
        chk_ascendant_fr: "I am parent/grandparent of a French citizen",
        chk_adopted_fr: "Adopted by a French citizen",
        chk_recueilli_fr: "Taken in as a minor by a French citizen",
        lbl_res_with_fr: "Years of residence with this French citizen",

        // Step 2.3: Education & Work
        step2_3_title: "Education & Work",
        lbl_education: "Highest education level",
        opt_edu_none: "No higher degree",
        opt_edu_bac3: "Bachelor's / Licence (Bac+3)",
        opt_edu_master_fr: "Master obtained in France (Bac+5)",
        opt_edu_master_foreign: "Master obtained abroad",
        opt_edu_phd: "PhD / Doctorate",
        lbl_job: "Professional Situation",
        opt_job_none: "Unemployed / Student",
        opt_job_cdi: "Permanent Contract (CDI)",
        opt_job_cdd: "Fixed-Term Contract (CDD)",
        opt_job_interim: "Interim",
        opt_job_freelance: "Freelance / Self-employed",

        // Step 2.4: Expert
        step2_4_title: "Specific Cases",
        chk_health: "Serious health issue requiring care",
        chk_refugee: "Recognized Refugee / Stateless",
        chk_no_oqtf: "I have no unrespected OQTF",
        btn_expert: "🛠️ Enable Advanced Options",
        txt_expert_intro: "The expert system will analyze 50+ additional criteria.",
        cat_military: "🎖️ Services & Military",
        chk_veteran: "Veteran",
        chk_combatant_card: "Combatant Card",
        chk_exc_services: "Exceptional Services",
        cat_health: "🏥 Health & Social",
        chk_pension: "Work Accident Pension",
        chk_child_care: "Child requiring care",

        // Navigation Buttons
        btn_new_sim: "← New Simulation",
        btn_submit: "Analyze my file",

        // Other Step 2 family details
        lbl_marriage_years: "Marriage duration (years)",
        lbl_spouse_nat: "Spouse Nationality",
        opt_spouse_foreign: "Foreign (Non-EU)",
        opt_spouse_fr: "French",
        opt_spouse_eu: "EU Citizen",
        chk_parent_fr_child: "I am the parent of a French child"
    },
    es: {
        app_title: "Simulegal",
        tagline: "Evalúe su elegibilidad para permisos de residencia en Francia",
        nav_login: "Iniciar sesión",
        nav_logout: "Cerrar sesión",
        nav_admin: "Administración",
        btn_chat: "🤖 Chatear con nuestra IA",
        step_situation: "Situación",
        step_criteres: "Criterios",
        step_resultats: "Resultados",
        btn_next: "Siguiente",
        btn_prev: "Anterior",
        btn_evaluate: "Analizar mi elegibilidad",
        locked_title: "🔒 Expediente Completo Bloqueado",
        locked_desc: "Para acceder a la lista precisa de documentos y descargar su informe oficial en PDF, desbloquee su expediente ahora.",
        btn_unlock: "Desbloquear Expediente (9.90€)",
        docs_required: "📄 Documentos necesarios",
        score_high: "Elegible",
        score_medium: "Parcial",
        score_low: "Poco probable"
    },
    ar: {
        app_title: "Simulegal",
        tagline: "قيم أهليتك للحصول على تصاريح الإقامة في فرنسا",
        nav_login: "تسجيل الدخول",
        nav_logout: "تسجيل الخروج",
        nav_admin: "الإدارة",
        btn_chat: "🤖 تحدث مع الذكاء الاصطناعي",
        step_situation: "الوضع الحالي",
        step_criteres: "المعايير",
        step_resultats: "النتائج",
        btn_next: "التالي",
        btn_prev: "السابق",
        btn_evaluate: "تحليل أهليتي",
        locked_title: "🔒 الملف الكامل مغلق",
        locked_desc: "للوصول إلى القائمة الدقيقة للمستندات وتحميل تقرير PDF الرسمي الخاص بك، قم بفتح ملفك الآن.",
        btn_unlock: "فتح الملف الكامل (9.90€)",
        docs_required: "📄 المستندات المطلوبة",
        score_high: "مؤهل",
        score_medium: "جزئي",
        score_low: "غير مرجح"
    }
};

const I18n = {
    currentLang: localStorage.getItem('simulegal_lang') || 'fr',

    init() {
        this.apply();
    },

    setLang(lang) {
        this.currentLang = lang;
        localStorage.setItem('simulegal_lang', lang);
        this.apply();
        // Optionnel: Recharger si nécessaire pour certains contenus dynamiques backend
    },

    t(key) {
        return translations[this.currentLang][key] || key;
    },

    apply() {
        // Appliquer RTL pour l'Arabe
        document.documentElement.dir = this.currentLang === 'ar' ? 'rtl' : 'ltr';
        document.documentElement.lang = this.currentLang;

        // Traduire tous les éléments avec data-i18n
        document.querySelectorAll('[data-i18n]').forEach(el => {
            const key = el.getAttribute('data-i18n');
            el.innerHTML = this.t(key);
        });

        // Traduire les placeholders
        document.querySelectorAll('[data-i18n-placeholder]').forEach(el => {
            const key = el.getAttribute('data-i18n-placeholder');
            el.placeholder = this.t(key);
        });

        // Synchroniser le selecteur de langue si présent
        const langSelect = document.getElementById('lang-select');
        if (langSelect) {
            langSelect.value = this.currentLang;
        }
    }
};

window.I18n = I18n;

// Initialisation automatique
if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', () => I18n.init());
} else {
    I18n.init();
}
