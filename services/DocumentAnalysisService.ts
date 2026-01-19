/**
 * Document Analysis Service
 * Simule l'OCR et la validation des documents
 */

export type AnalysisResult =
    | 'VALID'
    | 'REJECTED_BLURRY'
    | 'REJECTED_INCOMPLETE'
    | 'REJECTED_WRONG_TYPE'
    | 'REJECTED_EXPIRED';

export interface AnalysisResponse {
    status: AnalysisResult;
    confidence: number;     // 0-100
    message: string;
    extractedData?: Record<string, string>;
}

// Délai simulé d'analyse (1-3 secondes)
const ANALYSIS_DELAY = () => Math.random() * 2000 + 1000;

/**
 * Analyse un fichier uploadé et retourne le résultat de validation
 * Appel API vers /documents/analyze
 */
const DocumentAnalysisService = {
    analyze: async (file: File, docType?: string): Promise<AnalysisResponse> => {
    const API_URL = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:3001';

    try {
        const formData = new FormData();
        formData.append('file', file);
        if (docType) formData.append('docType', docType);

        const res = await fetch(`${API_URL}/documents/analyze`, {
            method: 'POST',
            body: formData
            // Note: fetch automatically sets Content-Type to multipart/form-data with boundary
        });

        if (!res.ok) {
            console.error('[OCR] ❌ Erreur analyse:', res.statusText);
            return {
                status: 'REJECTED_WRONG_TYPE', // Generic fallback
                confidence: 0,
                message: 'Erreur lors de l\'analyse du document.'
            };
        }

        const data: AnalysisResponse = await res.json();
        return data;

    } catch (error) {
        console.error('[OCR] ❌ Erreur réseau:', error);
        return {
            status: 'REJECTED_WRONG_TYPE',
            confidence: 0,
            message: 'Erreur de connexion au service d\'analyse.'
        };
    }
},

    /**
     * Vérifie si un résultat est valide
     */
    isValid: (result: AnalysisResult): boolean => {
        return result === 'VALID';
    },

        /**
         * Retourne le message d'erreur approprié
         */
        getErrorMessage: (result: AnalysisResult): string => {
            const messages: Record<AnalysisResult, string> = {
                VALID: '',
                REJECTED_BLURRY: 'Photo floue - Reprenez avec plus de lumière',
                REJECTED_INCOMPLETE: 'Document incomplet - Capturez toute la page',
                REJECTED_WRONG_TYPE: 'Mauvais document - Vérifiez le type demandé',
                REJECTED_EXPIRED: 'Document expiré - Fournissez un document valide'
            };
            return messages[result] || 'Erreur inconnue';
        },

            /**
             * Retourne l'icône appropriée pour le résultat
             */
            getResultIcon: (result: AnalysisResult): string => {
                if (result === 'VALID') return '✅';
                return '❌';
            }
};

export default DocumentAnalysisService;
