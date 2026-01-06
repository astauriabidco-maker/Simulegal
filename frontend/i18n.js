const translations = {
    fr: {
        app_title: "Simulegal",
        tagline: "Évaluez votre éligibilité aux titres de séjour en France",
        nav_login: "Se connecter",
        nav_logout: "Déconnexion",
        nav_admin: "Administration",
        nav_dashboard: "Mon Espace",
        nav_hello: "Bonjour",
        btn_chat: "🤖 Discuter avec notre IA",
        step_situation: "Situation",
        step_criteres: "Critères",
        step_resultats: "Résultats",
        btn_next: "Suivant",
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
        lbl_french: "Niveau de français (CECRL)"
    },
    en: {
        app_title: "Simulegal",
        tagline: "Assess your eligibility for French residence permits",
        nav_login: "Login",
        nav_logout: "Logout",
        nav_admin: "Admin",
        nav_dashboard: "My Dashboard",
        nav_hello: "Hello",
        btn_chat: "🤖 Chat with our AI",
        step_situation: "Situation",
        step_criteres: "Criteria",
        step_resultats: "Results",
        btn_next: "Next",
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
        lbl_french: "French level"
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
            el.textContent = this.t(key);
        });

        // Traduire les placeholders
        document.querySelectorAll('[data-i18n-placeholder]').forEach(el => {
            const key = el.getAttribute('data-i18n-placeholder');
            el.placeholder = this.t(key);
        });
    }
};

window.I18n = I18n;
