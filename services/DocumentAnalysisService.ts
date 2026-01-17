/**
 * Service d'analyse de documents (Simulation IA/OCR)
 * Imite le comportement d'une API comme Google Vision
 */

export type AnalysisStatus = 'VALID' | 'REJECTED_BLURRY' | 'REJECTED_EXPIRED' | 'REJECTED_WRONG_TYPE';

export interface ExtractedData {
    expiryDate?: string;
    docType?: string;
    fullName?: string;
    documentNumber?: string;
}

export interface AnalysisResult {
    isValid: boolean;
    status: AnalysisStatus;
    extractedData?: ExtractedData;
    message: string;
    messageAR?: string; // Traduction arabe
    messageEN?: string; // Traduction anglaise
    messageES?: string; // Traduction espagnole
    confidence?: number; // Score de confiance (0-100)
}

// Messages traduits pour chaque type de rejet
const REJECTION_MESSAGES = {
    REJECTED_BLURRY: {
        FR: "L'image est trop floue. Les caractères sont illisibles.",
        EN: "The image is too blurry. Characters are unreadable.",
        AR: "الصورة ضبابية جداً. الأحرف غير قابلة للقراءة.",
        ES: "La imagen está muy borrosa. Los caracteres son ilegibles."
    },
    REJECTED_EXPIRED: {
        FR: "Document expiré détecté.",
        EN: "Expired document detected.",
        AR: "تم اكتشاف مستند منتهي الصلاحية.",
        ES: "Documento caducado detectado."
    },
    REJECTED_WRONG_TYPE: {
        FR: "Le document ne correspond pas à la demande.",
        EN: "The document doesn't match the request.",
        AR: "المستند لا يتوافق مع الطلب.",
        ES: "El documento no corresponde con la solicitud."
    },
    VALID: {
        FR: "Document valide et lisible.",
        EN: "Valid and readable document.",
        AR: "مستند صالح وقابل للقراءة.",
        ES: "Documento válido y legible."
    }
};

export const DocumentAnalysis = {
    /**
     * Analyse un fichier uploadé et retourne le résultat
     * Simule une API d'OCR/Vision avec différents scénarios de test
     */
    analyze: async (file: File): Promise<AnalysisResult> => {
        // Simulation d'un délai réseau (l'IA réfléchit)
        const processingTime = 1500 + Math.random() * 1500; // 1.5s - 3s
        await new Promise(r => setTimeout(r, processingTime));

        const name = file.name.toLowerCase();
        const size = file.size;

        console.log(`[IA] 🔍 Analyse du document: ${file.name} (${(size / 1024).toFixed(1)} KB)`);

        // ============================================
        // SCÉNARIOS DE TEST (Basés sur le nom du fichier)
        // ============================================

        // 1. Cas "Flou / Illisible"
        if (name.includes('flou') || name.includes('blur') || name.includes('bad')) {
            console.log(`[IA] ❌ Rejet: Image floue détectée`);
            return {
                isValid: false,
                status: 'REJECTED_BLURRY',
                message: REJECTION_MESSAGES.REJECTED_BLURRY.FR,
                messageEN: REJECTION_MESSAGES.REJECTED_BLURRY.EN,
                messageAR: REJECTION_MESSAGES.REJECTED_BLURRY.AR,
                messageES: REJECTION_MESSAGES.REJECTED_BLURRY.ES,
                confidence: 15
            };
        }

        // 2. Cas "Périmé / Expiré"
        if (name.includes('perime') || name.includes('expired') || name.includes('old')) {
            console.log(`[IA] ❌ Rejet: Document expiré`);
            return {
                isValid: false,
                status: 'REJECTED_EXPIRED',
                extractedData: {
                    expiryDate: '2020-01-15',
                    docType: 'PASSPORT'
                },
                message: `${REJECTION_MESSAGES.REJECTED_EXPIRED.FR} (Date: 15/01/2020)`,
                messageEN: `${REJECTION_MESSAGES.REJECTED_EXPIRED.EN} (Date: 01/15/2020)`,
                messageAR: REJECTION_MESSAGES.REJECTED_EXPIRED.AR,
                messageES: `${REJECTION_MESSAGES.REJECTED_EXPIRED.ES} (Fecha: 15/01/2020)`,
                confidence: 92
            };
        }

        // 3. Cas "Mauvais document"
        if (name.includes('chat') || name.includes('vacances') || name.includes('selfie') || name.includes('cat')) {
            console.log(`[IA] ❌ Rejet: Mauvais type de document`);
            return {
                isValid: false,
                status: 'REJECTED_WRONG_TYPE',
                message: REJECTION_MESSAGES.REJECTED_WRONG_TYPE.FR,
                messageEN: REJECTION_MESSAGES.REJECTED_WRONG_TYPE.EN,
                messageAR: REJECTION_MESSAGES.REJECTED_WRONG_TYPE.AR,
                messageES: REJECTION_MESSAGES.REJECTED_WRONG_TYPE.ES,
                confidence: 88
            };
        }

        // 4. Cas très petite image (probablement de mauvaise qualité)
        if (size < 10000) { // Moins de 10KB
            console.log(`[IA] ❌ Rejet: Fichier trop petit (qualité insuffisante)`);
            return {
                isValid: false,
                status: 'REJECTED_BLURRY',
                message: "Le fichier est trop petit. Veuillez fournir une image de meilleure qualité.",
                messageEN: "File is too small. Please provide a higher quality image.",
                messageAR: "الملف صغير جداً. يرجى تقديم صورة بجودة أعلى.",
                messageES: "El archivo es demasiado pequeño. Proporcione una imagen de mejor calidad.",
                confidence: 10
            };
        }

        // ============================================
        // 5. Cas Succès (Par défaut)
        // ============================================
        console.log(`[IA] ✅ Document validé: ${file.name}`);

        // Génère des données extraites fictives
        const extractedData: ExtractedData = {
            expiryDate: '2030-12-31',
            docType: detectDocType(name),
            fullName: 'UTILISATEUR TEST',
            documentNumber: 'XX' + Math.random().toString(36).substr(2, 8).toUpperCase()
        };

        return {
            isValid: true,
            status: 'VALID',
            extractedData,
            message: REJECTION_MESSAGES.VALID.FR,
            messageEN: REJECTION_MESSAGES.VALID.EN,
            messageAR: REJECTION_MESSAGES.VALID.AR,
            messageES: REJECTION_MESSAGES.VALID.ES,
            confidence: 95 + Math.floor(Math.random() * 5) // 95-99%
        };
    },

    /**
     * Analyse rapide (pour preview instantané)
     */
    quickCheck: (file: File): { likely: 'valid' | 'invalid' } => {
        const name = file.name.toLowerCase();
        const badKeywords = ['flou', 'blur', 'bad', 'perime', 'expired', 'old', 'chat', 'vacances', 'selfie'];

        for (const keyword of badKeywords) {
            if (name.includes(keyword)) {
                return { likely: 'invalid' };
            }
        }

        return { likely: 'valid' };
    }
};

/**
 * Détecte le type de document basé sur le nom du fichier
 */
function detectDocType(filename: string): string {
    const name = filename.toLowerCase();

    if (name.includes('passport') || name.includes('passeport')) return 'PASSPORT';
    if (name.includes('carte') || name.includes('cni') || name.includes('id')) return 'ID_CARD';
    if (name.includes('domicile') || name.includes('address')) return 'PROOF_OF_ADDRESS';
    if (name.includes('photo')) return 'ID_PHOTO';
    if (name.includes('permis') || name.includes('license') || name.includes('driving')) return 'DRIVING_LICENSE';
    if (name.includes('timbre') || name.includes('stamp') || name.includes('fiscal')) return 'TAX_STAMP';
    if (name.includes('naissance') || name.includes('birth')) return 'BIRTH_CERTIFICATE';
    if (name.includes('impot') || name.includes('tax')) return 'TAX_NOTICE';

    return 'UNKNOWN';
}

export default DocumentAnalysis;
