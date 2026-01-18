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

export const DocumentAnalysisService = {
    /**
     * Analyse un fichier uploadé et retourne le résultat de validation
     * Simulation : rejette si le nom contient certains mots-clés
     */
    analyze: async (file: File, docType?: string): Promise<AnalysisResponse> => {
        console.log(`[OCR] 🔍 Analyse du fichier: ${file.name} (${docType || 'type inconnu'})`);

        // Simule un délai d'analyse
        await new Promise(resolve => setTimeout(resolve, ANALYSIS_DELAY()));

        const fileName = file.name.toLowerCase();

        // Règles de rejet basées sur le nom du fichier (simulation)
        if (fileName.includes('flou') || fileName.includes('blur') || fileName.includes('blurry')) {
            console.log('[OCR] ❌ Document flou détecté');
            return {
                status: 'REJECTED_BLURRY',
                confidence: 15,
                message: 'Le document est trop flou. Veuillez reprendre la photo avec un meilleur éclairage.'
            };
        }

        if (fileName.includes('incomplet') || fileName.includes('partial')) {
            return {
                status: 'REJECTED_INCOMPLETE',
                confidence: 30,
                message: 'Le document n\'est pas entièrement visible. Assurez-vous de capturer toute la page.'
            };
        }

        if (fileName.includes('expire') || fileName.includes('perime')) {
            return {
                status: 'REJECTED_EXPIRED',
                confidence: 85,
                message: 'Ce document semble être expiré. Veuillez fournir un document valide.'
            };
        }

        if (fileName.includes('mauvais') || fileName.includes('wrong')) {
            return {
                status: 'REJECTED_WRONG_TYPE',
                confidence: 90,
                message: 'Ce n\'est pas le bon type de document. Veuillez vérifier la demande.'
            };
        }

        // Par défaut : document valide
        console.log('[OCR] ✅ Document validé');
        return {
            status: 'VALID',
            confidence: 95,
            message: 'Document validé avec succès !',
            extractedData: {
                documentType: docType || 'unknown',
                analyzedAt: new Date().toISOString()
            }
        };
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
        return messages[result];
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
