'use client';

import React, { useState, useEffect } from 'react';
import { ServiceConfigStore, ServiceMetadata } from '../../services/ServiceConfigStore';
import { DocumentRequirement, DocumentCategory, OCRType } from '../../config/DocumentTemplates';
import {
    Settings,
    Save,
    Plus,
    X,
    Trash2,
    Edit2,
    FileText,
    Folder,
    ChevronRight,
    Check,
    AlertCircle,
    CheckCircle,
    Lock,
    Sparkles
} from 'lucide-react';

// Catégories disponibles
const CATEGORIES: { value: DocumentCategory; label: string }[] = [
    { value: 'IDENTITY', label: '🪪 Identité' },
    { value: 'RESIDENCE', label: '🏠 Résidence' },
    { value: 'FINANCIAL', label: '💰 Finances' },
    { value: 'CIVIL', label: '📜 État Civil' },
    { value: 'PROFESSIONAL', label: '💼 Professionnel' },
    { value: 'EDUCATION', label: '🎓 Éducation' },
    { value: 'OTHER', label: '📎 Autre' }
];

const OCR_TYPES: { value: OCRType | ''; label: string }[] = [
    { value: '', label: 'Aucun' },
    { value: 'PASSPORT', label: 'Passeport' },
    { value: 'ID_CARD', label: 'Carte d\'identité' },
    { value: 'TAX_NOTICE', label: 'Avis d\'imposition' },
    { value: 'RESIDENCE_PROOF', label: 'Justificatif de domicile' },
    { value: 'BIRTH_CERTIFICATE', label: 'Acte de naissance' },
    { value: 'PHOTO', label: 'Photo' },
    { value: 'DRIVING_LICENSE', label: 'Permis de conduire' },
    { value: 'GENERIC', label: 'Générique' }
];

const SERVICE_CATEGORIES: { value: 'IMMIGRATION' | 'DRIVING' | 'CIVIL' | 'OTHER'; label: string }[] = [
    { value: 'IMMIGRATION', label: '🛂 Immigration' },
    { value: 'DRIVING', label: '🚗 Permis' },
    { value: 'CIVIL', label: '📜 État Civil' },
    { value: 'OTHER', label: '📎 Autre' }
];

type ActiveTab = 'config' | 'documents' | 'services';

interface DocumentEditorProps {
    document?: DocumentRequirement;
    onSave: (doc: DocumentRequirement) => void;
    onClose: () => void;
}

function DocumentEditor({ document, onSave, onClose }: DocumentEditorProps) {
    const isEdit = !!document;
    const [formData, setFormData] = useState<Partial<DocumentRequirement>>({
        id: document?.id || '',
        label: document?.label || '',
        description: document?.description || '',
        category: document?.category || 'OTHER',
        ocrType: document?.ocrType,
        required: document?.required ?? true,
        maxSizeMB: document?.maxSizeMB || 10
    });
    const [error, setError] = useState('');

    const handleSubmit = (e: React.FormEvent) => {
        e.preventDefault();
        setError('');

        if (!formData.id || !formData.label || !formData.description) {
            setError('Veuillez remplir tous les champs obligatoires');
            return;
        }

        // Génère l'ID à partir du label si non fourni
        const docId = formData.id || formData.label.toUpperCase().replace(/[^A-Z0-9]/g, '_');

        const newDoc: DocumentRequirement = {
            id: docId,
            label: formData.label,
            description: formData.description,
            category: formData.category as DocumentCategory,
            ocrType: formData.ocrType || undefined,
            required: formData.required ?? true,
            maxSizeMB: formData.maxSizeMB
        };

        onSave(newDoc);
    };

    return (
        <div className="fixed inset-0 bg-black/50 flex items-center justify-center z-50 p-4">
            <div className="bg-white rounded-2xl shadow-2xl w-full max-w-lg max-h-[90vh] overflow-y-auto">
                <div className="p-6 border-b border-slate-100 flex items-center justify-between">
                    <h2 className="text-xl font-black text-slate-900">
                        {isEdit ? '✏️ Modifier le document' : '➕ Nouveau document'}
                    </h2>
                    <button onClick={onClose} className="text-slate-400 hover:text-slate-600">
                        <X size={24} />
                    </button>
                </div>

                <form onSubmit={handleSubmit} className="p-6 space-y-4">
                    {error && (
                        <div className="bg-red-50 text-red-700 p-3 rounded-xl text-sm flex items-center gap-2">
                            <AlertCircle size={16} />
                            {error}
                        </div>
                    )}

                    {!isEdit && (
                        <div>
                            <label className="block text-sm font-bold text-slate-700 mb-1">
                                Identifiant technique *
                            </label>
                            <input
                                type="text"
                                value={formData.id}
                                onChange={(e) => setFormData({ ...formData, id: e.target.value.toUpperCase().replace(/[^A-Z0-9_]/g, '_') })}
                                placeholder="Ex: ATTESTATION_CNAM"
                                className="w-full px-4 py-2 border border-slate-200 rounded-xl focus:ring-2 focus:ring-indigo-500 focus:border-transparent"
                            />
                            <p className="text-xs text-slate-400 mt-1">Lettres majuscules et underscores uniquement</p>
                        </div>
                    )}

                    <div>
                        <label className="block text-sm font-bold text-slate-700 mb-1">
                            Nom du document *
                        </label>
                        <input
                            type="text"
                            value={formData.label}
                            onChange={(e) => setFormData({ ...formData, label: e.target.value })}
                            placeholder="Ex: Attestation CNAM"
                            className="w-full px-4 py-2 border border-slate-200 rounded-xl focus:ring-2 focus:ring-indigo-500 focus:border-transparent"
                            required
                        />
                    </div>

                    <div>
                        <label className="block text-sm font-bold text-slate-700 mb-1">
                            Description *
                        </label>
                        <textarea
                            value={formData.description}
                            onChange={(e) => setFormData({ ...formData, description: e.target.value })}
                            placeholder="Ex: Attestation de droits à l'assurance maladie de moins de 3 mois."
                            rows={3}
                            className="w-full px-4 py-2 border border-slate-200 rounded-xl focus:ring-2 focus:ring-indigo-500 focus:border-transparent resize-none"
                            required
                        />
                    </div>

                    <div className="grid grid-cols-2 gap-4">
                        <div>
                            <label className="block text-sm font-bold text-slate-700 mb-1">
                                Catégorie
                            </label>
                            <select
                                value={formData.category}
                                onChange={(e) => setFormData({ ...formData, category: e.target.value as DocumentCategory })}
                                className="w-full px-4 py-2 border border-slate-200 rounded-xl focus:ring-2 focus:ring-indigo-500 focus:border-transparent"
                            >
                                {CATEGORIES.map(cat => (
                                    <option key={cat.value} value={cat.value}>{cat.label}</option>
                                ))}
                            </select>
                        </div>

                        <div>
                            <label className="block text-sm font-bold text-slate-700 mb-1">
                                Type OCR
                            </label>
                            <select
                                value={formData.ocrType || ''}
                                onChange={(e) => setFormData({ ...formData, ocrType: e.target.value as OCRType | undefined })}
                                className="w-full px-4 py-2 border border-slate-200 rounded-xl focus:ring-2 focus:ring-indigo-500 focus:border-transparent"
                            >
                                {OCR_TYPES.map(ocr => (
                                    <option key={ocr.value} value={ocr.value}>{ocr.label}</option>
                                ))}
                            </select>
                        </div>
                    </div>

                    <div className="flex items-center gap-3">
                        <input
                            type="checkbox"
                            id="required"
                            checked={formData.required}
                            onChange={(e) => setFormData({ ...formData, required: e.target.checked })}
                            className="w-5 h-5 rounded border-slate-300 text-indigo-600 focus:ring-indigo-500"
                        />
                        <label htmlFor="required" className="text-sm font-medium text-slate-700">
                            Document obligatoire
                        </label>
                    </div>

                    <div className="flex gap-3 pt-4">
                        <button
                            type="button"
                            onClick={onClose}
                            className="flex-1 px-4 py-3 bg-slate-100 hover:bg-slate-200 text-slate-700 rounded-xl font-bold transition-colors"
                        >
                            Annuler
                        </button>
                        <button
                            type="submit"
                            className="flex-1 px-4 py-3 bg-indigo-600 hover:bg-indigo-700 text-white rounded-xl font-bold transition-colors flex items-center justify-center gap-2"
                        >
                            <Save size={18} />
                            {isEdit ? 'Enregistrer' : 'Créer'}
                        </button>
                    </div>
                </form>
            </div>
        </div>
    );
}

interface ServiceEditorProps {
    service?: ServiceMetadata;
    allServices: ServiceMetadata[];
    onSave: (service: ServiceMetadata) => void;
    onClose: () => void;
}

function ServiceEditor({ service, allServices, onSave, onClose }: ServiceEditorProps) {
    const isEdit = !!service;
    const [formData, setFormData] = useState<Partial<ServiceMetadata>>({
        id: service?.id || '',
        name: service?.name || '',
        description: service?.description || '',
        category: service?.category || 'IMMIGRATION',
        parentId: service?.parentId
    });
    const [error, setError] = useState('');

    const handleSubmit = (e: React.FormEvent) => {
        e.preventDefault();
        setError('');

        if (!formData.id || !formData.name || !formData.description) {
            setError('Veuillez remplir tous les champs obligatoires');
            return;
        }

        const newService: ServiceMetadata = {
            id: formData.id,
            name: formData.name,
            description: formData.description,
            category: formData.category as ServiceMetadata['category'],
            parentId: formData.parentId || undefined
        };

        onSave(newService);
    };

    return (
        <div className="fixed inset-0 bg-black/50 flex items-center justify-center z-50 p-4">
            <div className="bg-white rounded-2xl shadow-2xl w-full max-w-lg max-h-[90vh] overflow-y-auto">
                <div className="p-6 border-b border-slate-100 flex items-center justify-between">
                    <h2 className="text-xl font-black text-slate-900">
                        {isEdit ? '✏️ Modifier le service' : '➕ Nouveau service'}
                    </h2>
                    <button onClick={onClose} className="text-slate-400 hover:text-slate-600">
                        <X size={24} />
                    </button>
                </div>

                <form onSubmit={handleSubmit} className="p-6 space-y-4">
                    {error && (
                        <div className="bg-red-50 text-red-700 p-3 rounded-xl text-sm flex items-center gap-2">
                            <AlertCircle size={16} />
                            {error}
                        </div>
                    )}

                    {!isEdit && (
                        <div>
                            <label className="block text-sm font-bold text-slate-700 mb-1">
                                Identifiant technique *
                            </label>
                            <input
                                type="text"
                                value={formData.id}
                                onChange={(e) => setFormData({ ...formData, id: e.target.value.toLowerCase().replace(/[^a-z0-9_]/g, '_') })}
                                placeholder="Ex: titre_sejour_special"
                                className="w-full px-4 py-2 border border-slate-200 rounded-xl focus:ring-2 focus:ring-indigo-500 focus:border-transparent"
                            />
                            <p className="text-xs text-slate-400 mt-1">Lettres minuscules et underscores uniquement</p>
                        </div>
                    )}

                    <div>
                        <label className="block text-sm font-bold text-slate-700 mb-1">
                            Nom du service *
                        </label>
                        <input
                            type="text"
                            value={formData.name}
                            onChange={(e) => setFormData({ ...formData, name: e.target.value })}
                            placeholder="Ex: 🎫 Titre de Séjour Spécial"
                            className="w-full px-4 py-2 border border-slate-200 rounded-xl focus:ring-2 focus:ring-indigo-500 focus:border-transparent"
                            required
                        />
                        <p className="text-xs text-slate-400 mt-1">Vous pouvez utiliser des emojis</p>
                    </div>

                    <div>
                        <label className="block text-sm font-bold text-slate-700 mb-1">
                            Description *
                        </label>
                        <textarea
                            value={formData.description}
                            onChange={(e) => setFormData({ ...formData, description: e.target.value })}
                            placeholder="Ex: Description courte du service pour les admins"
                            rows={2}
                            className="w-full px-4 py-2 border border-slate-200 rounded-xl focus:ring-2 focus:ring-indigo-500 focus:border-transparent resize-none"
                            required
                        />
                    </div>

                    <div className="grid grid-cols-2 gap-4">
                        <div>
                            <label className="block text-sm font-bold text-slate-700 mb-1">
                                Catégorie
                            </label>
                            <select
                                value={formData.category}
                                onChange={(e) => setFormData({ ...formData, category: e.target.value as ServiceMetadata['category'] })}
                                className="w-full px-4 py-2 border border-slate-200 rounded-xl focus:ring-2 focus:ring-indigo-500 focus:border-transparent"
                            >
                                {SERVICE_CATEGORIES.map(cat => (
                                    <option key={cat.value} value={cat.value}>{cat.label}</option>
                                ))}
                            </select>
                        </div>

                        <div>
                            <label className="block text-sm font-bold text-slate-700 mb-1">
                                Service Parent
                            </label>
                            <select
                                value={formData.parentId || ''}
                                onChange={(e) => setFormData({ ...formData, parentId: e.target.value || undefined })}
                                className="w-full px-4 py-2 border border-slate-200 rounded-xl focus:ring-2 focus:ring-indigo-500 focus:border-transparent"
                            >
                                <option value="">Aucun (service racine)</option>
                                {allServices
                                    .filter(s => s.id !== formData.id)
                                    .map(s => (
                                        <option key={s.id} value={s.id}>{s.name}</option>
                                    ))
                                }
                            </select>
                        </div>
                    </div>

                    <div className="flex gap-3 pt-4">
                        <button
                            type="button"
                            onClick={onClose}
                            className="flex-1 px-4 py-3 bg-slate-100 hover:bg-slate-200 text-slate-700 rounded-xl font-bold transition-colors"
                        >
                            Annuler
                        </button>
                        <button
                            type="submit"
                            className="flex-1 px-4 py-3 bg-indigo-600 hover:bg-indigo-700 text-white rounded-xl font-bold transition-colors flex items-center justify-center gap-2"
                        >
                            <Save size={18} />
                            {isEdit ? 'Enregistrer' : 'Créer'}
                        </button>
                    </div>
                </form>
            </div>
        </div>
    );
}

export default function ServiceConfigPanelV2() {
    const [activeTab, setActiveTab] = useState<ActiveTab>('config');
    const [services, setServices] = useState<ServiceMetadata[]>([]);
    const [documents, setDocuments] = useState<DocumentRequirement[]>([]);
    const [showNotification, setShowNotification] = useState(false);
    const [notificationMessage, setNotificationMessage] = useState('');

    // Document editor state
    const [showDocEditor, setShowDocEditor] = useState(false);
    const [editingDoc, setEditingDoc] = useState<DocumentRequirement | undefined>();

    // Service editor state
    const [showServiceEditor, setShowServiceEditor] = useState(false);
    const [editingService, setEditingService] = useState<ServiceMetadata | undefined>();

    // Config state (from original panel)
    const [selectedService, setSelectedService] = useState<string>('naturalisation');
    const [currentConfig, setCurrentConfig] = useState<string[]>([]);
    const [hasChanges, setHasChanges] = useState(false);

    // Charge les données au montage
    useEffect(() => {
        setServices(ServiceConfigStore.getAllServices());
        setDocuments(ServiceConfigStore.getAllDocuments());
    }, []);

    // Charge la configuration du service sélectionné
    useEffect(() => {
        const config = ServiceConfigStore.getServiceConfig(selectedService);
        setCurrentConfig(config);
        setHasChanges(false);
    }, [selectedService]);

    const showNotificationMsg = (message: string) => {
        setNotificationMessage(message);
        setShowNotification(true);
        setTimeout(() => setShowNotification(false), 3000);
    };

    // ============ DOCUMENTS ============

    const handleAddDocument = () => {
        setEditingDoc(undefined);
        setShowDocEditor(true);
    };

    const handleEditDocument = (doc: DocumentRequirement) => {
        setEditingDoc(doc);
        setShowDocEditor(true);
    };

    const handleSaveDocument = (doc: DocumentRequirement) => {
        if (editingDoc) {
            ServiceConfigStore.updateDocument(editingDoc.id, doc);
            showNotificationMsg(`✅ Document "${doc.label}" mis à jour`);
        } else {
            const success = ServiceConfigStore.addDocument(doc);
            if (!success) {
                showNotificationMsg(`❌ Erreur: document "${doc.id}" existe déjà`);
                return;
            }
            showNotificationMsg(`✅ Document "${doc.label}" créé`);
        }
        setDocuments(ServiceConfigStore.getAllDocuments());
        setShowDocEditor(false);
    };

    const handleDeleteDocument = (docId: string) => {
        if (!confirm('Êtes-vous sûr de vouloir supprimer ce document ?')) return;

        const success = ServiceConfigStore.deleteDocument(docId);
        if (success) {
            showNotificationMsg('✅ Document supprimé');
            setDocuments(ServiceConfigStore.getAllDocuments());
        } else {
            showNotificationMsg('❌ Impossible de supprimer ce document');
        }
    };

    // ============ SERVICES ============

    const handleAddService = () => {
        setEditingService(undefined);
        setShowServiceEditor(true);
    };

    const handleEditService = (service: ServiceMetadata) => {
        setEditingService(service);
        setShowServiceEditor(true);
    };

    const handleSaveService = (service: ServiceMetadata) => {
        if (editingService) {
            ServiceConfigStore.updateService(editingService.id, service);
            showNotificationMsg(`✅ Service "${service.name}" mis à jour`);
        } else {
            const success = ServiceConfigStore.addService(service);
            if (!success) {
                showNotificationMsg(`❌ Erreur: service "${service.id}" existe déjà`);
                return;
            }
            showNotificationMsg(`✅ Service "${service.name}" créé`);
        }
        setServices(ServiceConfigStore.getAllServices());
        setShowServiceEditor(false);
    };

    const handleDeleteService = (serviceId: string) => {
        if (!confirm('Êtes-vous sûr de vouloir supprimer ce service ?')) return;

        const success = ServiceConfigStore.deleteService(serviceId);
        if (success) {
            showNotificationMsg('✅ Service supprimé');
            setServices(ServiceConfigStore.getAllServices());
        } else {
            showNotificationMsg('❌ Impossible de supprimer ce service');
        }
    };

    // ============ CONFIG ============

    const toggleDocument = (docId: string) => {
        setCurrentConfig(prev => {
            if (prev.includes(docId)) {
                return prev.filter(id => id !== docId);
            } else {
                return [...prev, docId];
            }
        });
        setHasChanges(true);
    };

    const handleSaveConfig = () => {
        ServiceConfigStore.updateServiceConfig(selectedService, currentConfig);
        setHasChanges(false);
        showNotificationMsg('✅ Configuration sauvegardée');
    };

    // Grouper les documents par catégorie
    const documentsByCategory = documents.reduce((acc, doc) => {
        const category = doc.category || 'OTHER';
        if (!acc[category]) acc[category] = [];
        acc[category].push(doc);
        return acc;
    }, {} as Record<string, DocumentRequirement[]>);

    return (
        <div className="h-full flex flex-col bg-slate-100">
            {/* Header */}
            <div className="bg-white border-b border-slate-200 p-6">
                <div className="flex items-center justify-between">
                    <div className="flex items-center gap-3">
                        <div className="w-12 h-12 bg-indigo-100 rounded-xl flex items-center justify-center">
                            <Settings className="text-indigo-600" size={24} />
                        </div>
                        <div>
                            <h1 className="text-2xl font-black text-slate-900">Gestion Dynamique</h1>
                            <p className="text-slate-500 text-sm">Documents, Services et Configuration</p>
                        </div>
                    </div>

                    {/* Tabs */}
                    <div className="flex bg-slate-100 p-1 rounded-xl">
                        <button
                            onClick={() => setActiveTab('config')}
                            className={`px-4 py-2 rounded-lg font-bold text-sm transition-all ${activeTab === 'config'
                                    ? 'bg-indigo-600 text-white shadow'
                                    : 'text-slate-600 hover:text-slate-900'
                                }`}
                        >
                            ⚙️ Configuration
                        </button>
                        <button
                            onClick={() => setActiveTab('documents')}
                            className={`px-4 py-2 rounded-lg font-bold text-sm transition-all ${activeTab === 'documents'
                                    ? 'bg-emerald-600 text-white shadow'
                                    : 'text-slate-600 hover:text-slate-900'
                                }`}
                        >
                            📄 Documents ({documents.length})
                        </button>
                        <button
                            onClick={() => setActiveTab('services')}
                            className={`px-4 py-2 rounded-lg font-bold text-sm transition-all ${activeTab === 'services'
                                    ? 'bg-amber-600 text-white shadow'
                                    : 'text-slate-600 hover:text-slate-900'
                                }`}
                        >
                            📋 Services ({services.length})
                        </button>
                    </div>
                </div>
            </div>

            {/* Content */}
            <div className="flex-1 overflow-auto p-6">
                {/* Tab: Documents */}
                {activeTab === 'documents' && (
                    <div>
                        <div className="flex items-center justify-between mb-6">
                            <h2 className="text-xl font-black text-slate-900">
                                Catalogue des Documents
                            </h2>
                            <button
                                onClick={handleAddDocument}
                                className="flex items-center gap-2 px-4 py-2 bg-emerald-600 hover:bg-emerald-700 text-white rounded-xl font-bold transition-colors"
                            >
                                <Plus size={18} />
                                Nouveau document
                            </button>
                        </div>

                        <div className="grid gap-4">
                            {Object.entries(documentsByCategory).map(([category, docs]) => (
                                <div key={category} className="bg-white rounded-2xl border border-slate-100 overflow-hidden">
                                    <div className="p-4 bg-slate-50 border-b border-slate-100">
                                        <h3 className="font-bold text-slate-700">
                                            {CATEGORIES.find(c => c.value === category)?.label || category}
                                            <span className="ml-2 text-sm font-normal text-slate-400">
                                                ({docs.length} documents)
                                            </span>
                                        </h3>
                                    </div>
                                    <div className="divide-y divide-slate-50">
                                        {docs.map(doc => {
                                            const isCustom = ServiceConfigStore.isCustomDocument(doc.id);
                                            return (
                                                <div key={doc.id} className="p-4 flex items-center justify-between hover:bg-slate-50 transition-colors">
                                                    <div className="flex items-center gap-3">
                                                        <div className={`w-10 h-10 rounded-xl flex items-center justify-center ${isCustom ? 'bg-emerald-100' : 'bg-slate-100'}`}>
                                                            <FileText size={18} className={isCustom ? 'text-emerald-600' : 'text-slate-400'} />
                                                        </div>
                                                        <div>
                                                            <p className="font-bold text-slate-900">{doc.label}</p>
                                                            <p className="text-xs text-slate-400">{doc.id}</p>
                                                        </div>
                                                        {doc.ocrType && (
                                                            <span className="text-xs font-bold px-2 py-0.5 rounded-full bg-purple-100 text-purple-700 flex items-center gap-1">
                                                                <Sparkles size={10} />
                                                                OCR
                                                            </span>
                                                        )}
                                                        {!isCustom && (
                                                            <span className="text-xs font-bold px-2 py-0.5 rounded-full bg-slate-100 text-slate-500 flex items-center gap-1">
                                                                <Lock size={10} />
                                                                Par défaut
                                                            </span>
                                                        )}
                                                    </div>
                                                    {isCustom && (
                                                        <div className="flex items-center gap-2">
                                                            <button
                                                                onClick={() => handleEditDocument(doc)}
                                                                className="p-2 text-slate-400 hover:text-indigo-600 transition-colors"
                                                            >
                                                                <Edit2 size={16} />
                                                            </button>
                                                            <button
                                                                onClick={() => handleDeleteDocument(doc.id)}
                                                                className="p-2 text-slate-400 hover:text-red-600 transition-colors"
                                                            >
                                                                <Trash2 size={16} />
                                                            </button>
                                                        </div>
                                                    )}
                                                </div>
                                            );
                                        })}
                                    </div>
                                </div>
                            ))}
                        </div>
                    </div>
                )}

                {/* Tab: Services */}
                {activeTab === 'services' && (
                    <div>
                        <div className="flex items-center justify-between mb-6">
                            <h2 className="text-xl font-black text-slate-900">
                                Catalogue des Services
                            </h2>
                            <button
                                onClick={handleAddService}
                                className="flex items-center gap-2 px-4 py-2 bg-amber-600 hover:bg-amber-700 text-white rounded-xl font-bold transition-colors"
                            >
                                <Plus size={18} />
                                Nouveau service
                            </button>
                        </div>

                        <div className="grid gap-3">
                            {services.map(service => {
                                const isCustom = ServiceConfigStore.isCustomService(service.id);
                                const docCount = ServiceConfigStore.getServiceConfig(service.id).length;

                                return (
                                    <div key={service.id} className="bg-white rounded-2xl border border-slate-100 p-4 flex items-center justify-between hover:shadow-lg transition-shadow">
                                        <div className="flex items-center gap-4">
                                            <div className={`w-12 h-12 rounded-xl flex items-center justify-center ${isCustom ? 'bg-amber-100' : 'bg-slate-100'}`}>
                                                <Folder size={20} className={isCustom ? 'text-amber-600' : 'text-slate-400'} />
                                            </div>
                                            <div>
                                                <p className="font-bold text-slate-900">{service.name}</p>
                                                <p className="text-xs text-slate-400">{service.description}</p>
                                            </div>
                                            <span className="text-xs font-bold px-2 py-0.5 rounded-full bg-indigo-100 text-indigo-700">
                                                {docCount} docs
                                            </span>
                                            {service.parentId && (
                                                <span className="text-xs font-bold px-2 py-0.5 rounded-full bg-slate-100 text-slate-500">
                                                    Hérite de: {service.parentId}
                                                </span>
                                            )}
                                            {!isCustom && (
                                                <span className="text-xs font-bold px-2 py-0.5 rounded-full bg-slate-100 text-slate-500 flex items-center gap-1">
                                                    <Lock size={10} />
                                                    Par défaut
                                                </span>
                                            )}
                                        </div>
                                        {isCustom && (
                                            <div className="flex items-center gap-2">
                                                <button
                                                    onClick={() => handleEditService(service)}
                                                    className="p-2 text-slate-400 hover:text-indigo-600 transition-colors"
                                                >
                                                    <Edit2 size={16} />
                                                </button>
                                                <button
                                                    onClick={() => handleDeleteService(service.id)}
                                                    className="p-2 text-slate-400 hover:text-red-600 transition-colors"
                                                >
                                                    <Trash2 size={16} />
                                                </button>
                                            </div>
                                        )}
                                    </div>
                                );
                            })}
                        </div>
                    </div>
                )}

                {/* Tab: Config (simplified version from original) */}
                {activeTab === 'config' && (
                    <div className="flex gap-6">
                        {/* Liste des services */}
                        <div className="w-80 bg-white rounded-2xl border border-slate-100 overflow-hidden">
                            <div className="p-4 bg-slate-50 border-b border-slate-100">
                                <h3 className="font-bold text-slate-700">Services</h3>
                            </div>
                            <div className="p-2 max-h-[60vh] overflow-y-auto">
                                {services.map(service => (
                                    <button
                                        key={service.id}
                                        onClick={() => setSelectedService(service.id)}
                                        className={`w-full text-left p-3 rounded-xl transition-all mb-1 ${selectedService === service.id
                                                ? 'bg-indigo-600 text-white'
                                                : 'hover:bg-slate-50 text-slate-700'
                                            }`}
                                    >
                                        <p className="font-bold text-sm truncate">{service.name}</p>
                                        <p className={`text-xs ${selectedService === service.id ? 'text-white/70' : 'text-slate-400'}`}>
                                            {ServiceConfigStore.getServiceConfig(service.id).length} documents
                                        </p>
                                    </button>
                                ))}
                            </div>
                        </div>

                        {/* Configuration */}
                        <div className="flex-1">
                            <div className="flex items-center justify-between mb-4">
                                <h2 className="text-xl font-black text-slate-900">
                                    Documents pour : {services.find(s => s.id === selectedService)?.name}
                                </h2>
                                <button
                                    onClick={handleSaveConfig}
                                    disabled={!hasChanges}
                                    className={`flex items-center gap-2 px-4 py-2 rounded-xl font-bold transition-all ${hasChanges
                                            ? 'bg-emerald-600 hover:bg-emerald-700 text-white shadow-lg'
                                            : 'bg-slate-100 text-slate-400 cursor-not-allowed'
                                        }`}
                                >
                                    <Save size={16} />
                                    Enregistrer
                                </button>
                            </div>

                            <div className="grid grid-cols-2 gap-3">
                                {documents.map(doc => {
                                    const isSelected = currentConfig.includes(doc.id);
                                    return (
                                        <button
                                            key={doc.id}
                                            onClick={() => toggleDocument(doc.id)}
                                            className={`p-4 rounded-xl border-2 text-left transition-all ${isSelected
                                                    ? 'border-indigo-500 bg-indigo-50'
                                                    : 'border-slate-200 bg-white hover:border-slate-300'
                                                }`}
                                        >
                                            <div className="flex items-start gap-3">
                                                <div className={`w-6 h-6 rounded-lg flex items-center justify-center flex-shrink-0 ${isSelected ? 'bg-indigo-600 text-white' : 'bg-slate-100'
                                                    }`}>
                                                    {isSelected && <Check size={14} />}
                                                </div>
                                                <div>
                                                    <p className={`font-bold text-sm ${isSelected ? 'text-indigo-900' : 'text-slate-900'}`}>
                                                        {doc.label}
                                                    </p>
                                                    <p className={`text-xs ${isSelected ? 'text-indigo-600' : 'text-slate-400'}`}>
                                                        {doc.id}
                                                    </p>
                                                </div>
                                            </div>
                                        </button>
                                    );
                                })}
                            </div>
                        </div>
                    </div>
                )}
            </div>

            {/* Modals */}
            {showDocEditor && (
                <DocumentEditor
                    document={editingDoc}
                    onSave={handleSaveDocument}
                    onClose={() => setShowDocEditor(false)}
                />
            )}

            {showServiceEditor && (
                <ServiceEditor
                    service={editingService}
                    allServices={services}
                    onSave={handleSaveService}
                    onClose={() => setShowServiceEditor(false)}
                />
            )}

            {/* Notification Toast */}
            {showNotification && (
                <div className="fixed bottom-6 right-6 bg-slate-900 text-white px-6 py-4 rounded-2xl shadow-2xl flex items-center gap-3 z-50">
                    <CheckCircle className="text-emerald-400" size={20} />
                    <span className="font-bold">{notificationMessage}</span>
                </div>
            )}

            {/* Unsaved changes warning */}
            {hasChanges && activeTab === 'config' && (
                <div className="fixed bottom-6 left-1/2 -translate-x-1/2 bg-indigo-600 text-white px-6 py-4 rounded-2xl shadow-2xl flex items-center gap-4 z-40">
                    <AlertCircle size={20} />
                    <span className="font-bold">Modifications non sauvegardées</span>
                    <button
                        onClick={handleSaveConfig}
                        className="bg-white text-indigo-600 px-4 py-2 rounded-xl font-black text-sm hover:bg-indigo-50"
                    >
                        Sauvegarder
                    </button>
                </div>
            )}
        </div>
    );
}
