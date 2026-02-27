const fs = require('fs');
const path = require('path');

const filePath = path.join(__dirname, 'components/sales/SalesDashboard.tsx');
let content = fs.readFileSync(filePath, 'utf-8');
const lines = content.split('\n');

const startIdx = lines.findIndex(l => l.includes('{/* Drawer Detail — Fiche Prospect Enrichie */}'));
// find the matching closing brace for selectedProspect && (
let openBraces = 0;
let endIdx = -1;

for (let i = startIdx; i < lines.length; i++) {
    if (lines[i].includes('selectedProspect && (')) {
        openBraces++;
    }
    // we just need to find the `)}` that matches the `selectedProspect && (`
    // which is at line 2069/2070. Let's just look for the literal `)}` at the right indentation
    if (lines[i].trim() === ')}' && lines[i-1].trim() === '</div>' && lines[i-2].trim() === '</div>') {
        if (i > startIdx + 100) {
            endIdx = i;
            break;
        }
    }
}

if (startIdx === -1 || endIdx === -1) {
    console.error('Drawer not found', startIdx, endIdx);
    process.exit(1);
}

console.log('Start:', startIdx, 'End:', endIdx);

const drawerCode = lines.slice(startIdx, endIdx + 1).join('\n');

const prospectDrawerContent = `import React, { useState, useEffect } from 'react';
import { useRouter } from 'next/navigation';
import { 
    XCircle, Phone, Mail, MapPin, CheckCircle, Calendar, Microscope, Flame, ArrowRight 
} from 'lucide-react';
import { SalesStore } from '../../services/SalesStore';
import { Prospect, ProspectNote } from '../../services/SalesStore';
import { WhatsAppWidget } from '../backoffice/WhatsAppWidget';
import { SERVICES_CATALOG } from '../../data/services';

// ─── Boutons d'encaissement avec prix backend ───
export function PaymentButtons({ prospect, onPay, fetchPrice }: {
    prospect: Prospect;
    onPay: (prospect: Prospect, installments: 1 | 3) => void;
    fetchPrice: (serviceId: string) => Promise<{ priceEuros: number; pricePer3: number; serviceName: string; source: string; promoActive: boolean }>;
}) {
    const [pricing, setPricing] = React.useState<{ priceEuros: number; pricePer3: number; promoActive: boolean } | null>(null);

    React.useEffect(() => {
        const serviceId = prospect.eligibilityResult?.matchedProcedures?.[0] || prospect.interestServiceId || '';
        if (serviceId) {
            fetchPrice(serviceId).then(setPricing);
        }
    }, [prospect.id, prospect.interestServiceId]);

    if (!pricing) {
        return (
            <div className="flex gap-2">
                <div className="flex-1 py-2.5 bg-slate-100 rounded-xl animate-pulse" />
                <div className="flex-1 py-2.5 bg-slate-100 rounded-xl animate-pulse" />
            </div>
        );
    }

    return (
        <div className="flex gap-2">
            <button
                onClick={() => onPay(prospect, 1)}
                className="flex-1 flex items-center justify-center gap-2 py-2.5 bg-emerald-600 text-white rounded-xl text-sm font-bold hover:bg-emerald-700 transition-all shadow-sm shadow-emerald-200 active:scale-[0.97]"
            >
                💳 Encaisser {pricing.priceEuros}€
                {pricing.promoActive && <span className="ml-1 px-1 py-0.5 bg-red-500 text-[9px] rounded-md uppercase">Promo</span>}
            </button>
            <button
                onClick={() => onPay(prospect, 3)}
                className="flex-1 flex items-center justify-center gap-2 py-2.5 bg-white border border-emerald-300 text-emerald-700 rounded-xl text-sm font-bold hover:bg-emerald-50 transition-all active:scale-[0.97]"
            >
                💳 3x {pricing.pricePer3}€
            </button>
        </div>
    );
}

// ─── Affichage informatif du prix backend dans le drawer ───
export function ServicePriceDisplay({ serviceId, fetchPrice }: {
    serviceId: string;
    fetchPrice: (serviceId: string) => Promise<{ priceEuros: number; pricePer3: number; serviceName: string; source: string; promoActive: boolean }>;
}) {
    const [pricing, setPricing] = React.useState<{ priceEuros: number; serviceName: string; source: string; promoActive: boolean } | null>(null);

    React.useEffect(() => {
        if (serviceId) {
            fetchPrice(serviceId).then(setPricing);
        }
    }, [serviceId]);

    if (!serviceId || !pricing) return null;

    const sourceLabel = pricing.source === 'PROMO' ? '🏷️ Promo' :
        pricing.source === 'ADMIN_OVERRIDE' ? '⚙️ Admin' :
            pricing.source === 'CATALOG_DEFAULT' || pricing.source === 'FRONTEND_MAP' ? '📋 Défaut' : '';

    const sourceColor = pricing.source === 'PROMO' ? 'text-red-500' :
        pricing.source === 'ADMIN_OVERRIDE' ? 'text-indigo-500' : 'text-slate-400';

    return (
        <div className="flex items-center gap-2 mt-1">
            <span className="text-sm font-black text-emerald-600">{pricing.priceEuros}€</span>
            {sourceLabel && <span className={\`text-[9px] font-bold \${sourceColor}\`}>{sourceLabel}</span>}
            {pricing.promoActive && <span className="px-1.5 py-0.5 bg-red-100 text-red-600 text-[9px] font-bold rounded-md animate-pulse">PROMO ACTIVE</span>}
        </div>
    );
}

export default function ProspectDrawer({
    selectedProspect,
    setSelectedProspect,
    setProspects,
    loadProspects,
    COLUMNS,
    setShowCallCockpit,
    setShowBookingModal,
    setShowSimulatorModal,
    setShowFollowUpModal,
    setShowTagsModal,
    handleStatusChange,
    handleNoShow,
    handleStartPayment,
    fetchServicePrice,
    handleCancelAppointment,
    handleMarkAsLost,
    handleDeleteProspect,
    loadTimeline,
    setConfirmDialog,
    showToast,
    callHistory,
}: any) {
    const router = useRouter();
    const [isEditingInfo, setIsEditingInfo] = useState(false);
    const [editForm, setEditForm] = useState({ firstName: '', lastName: '', phone: '', email: '', address: '', city: '', zipCode: '', country: 'France' });
    const [isEditingMarketing, setIsEditingMarketing] = useState(false);
    const [marketingForm, setMarketingForm] = useState({ source: 'WEBSITE', campaignName: '', interestServiceId: '' });
    const [isLoading, setIsLoading] = useState(false);
    const [newNote, setNewNote] = useState('');
    const [callbackSchedule, setCallbackSchedule] = useState('');

    useEffect(() => {
        if (selectedProspect) {
            setEditForm({
                firstName: selectedProspect.firstName || '',
                lastName: selectedProspect.lastName || '',
                phone: selectedProspect.phone || '',
                email: selectedProspect.email || '',
                address: selectedProspect.address || '',
                city: selectedProspect.city || '',
                zipCode: selectedProspect.zipCode || '',
                country: selectedProspect.country || 'France'
            });
            setMarketingForm({
                source: selectedProspect.source || 'WEBSITE',
                campaignName: selectedProspect.campaignName || '',
                interestServiceId: selectedProspect.interestServiceId || ''
            });
        }
    }, [selectedProspect?.id]);

    const handleSaveInfo = async () => {
        if (!selectedProspect) return;
        setIsLoading(true);
        await SalesStore.updateProspect(selectedProspect.id, editForm);
        setSelectedProspect((prev: any) => prev ? { ...prev, ...editForm } : null);
        setProspects((prev: any) => prev.map((p: any) => p.id === selectedProspect.id ? { ...p, ...editForm } : p));
        setIsEditingInfo(false);
        setIsLoading(false);
    };

    const handleSaveMarketing = async () => {
        if (!selectedProspect) return;
        setIsLoading(true);
        await SalesStore.updateProspect(selectedProspect.id, marketingForm as any);
        setSelectedProspect((prev: any) => prev ? { ...prev, ...marketingForm } as any : null);
        setProspects((prev: any) => prev.map((p: any) => p.id === selectedProspect.id ? { ...p, ...marketingForm } as any : p));
        setIsEditingMarketing(false);
        setIsLoading(false);
    };

    const handleQuickCallOutcome = async (outcome: 'NO_ANSWER' | 'CALLBACK' | 'NOT_INTERESTED' | 'WRONG_NUMBER' | 'INTERESTED') => {
        if (!selectedProspect) return;

        const MAX_NO_ANSWER = 5;
        const MAX_CALLBACKS = 3;

        const updates: any = {};
        const newCallAttempts = (selectedProspect.callAttempts || 0) + 1;
        let newNoAnswerCount = selectedProspect.noAnswerCount || 0;
        let newCallbackCount = selectedProspect.callbackCount || 0;

        updates.callAttempts = newCallAttempts;
        updates.lastCallOutcome = outcome;
        updates.lastContactAt = new Date().toISOString();

        if (outcome === 'NO_ANSWER') {
            newNoAnswerCount++;
            updates.noAnswerCount = newNoAnswerCount;
        } else {
            updates.noAnswerCount = 0;
            newNoAnswerCount = 0;
        }

        if (outcome === 'CALLBACK') {
            newCallbackCount++;
            updates.callbackCount = newCallbackCount;
            updates.callbackRequestedAt = new Date().toISOString();
            if (callbackSchedule) {
                updates.callbackScheduledAt = new Date(callbackSchedule).toISOString();
            }
        }

        let autoLostReason = '';

        if (outcome === 'NOT_INTERESTED' || outcome === 'WRONG_NUMBER') {
            updates.status = 'LOST';
            autoLostReason = outcome === 'NOT_INTERESTED' ? 'Pas intéressé' : 'Mauvais numéro';
            updates.lostReason = autoLostReason;
        } else if (outcome === 'INTERESTED') {
            updates.status = 'QUALIFIED';
            updates.lastCallOutcome = 'INTERESTED';
        } else if (newNoAnswerCount >= MAX_NO_ANSWER) {
            updates.status = 'LOST';
            autoLostReason = \`\${newNoAnswerCount} appels sans réponse consécutifs\`;
            updates.lostReason = autoLostReason;
        } else if (newCallbackCount >= MAX_CALLBACKS) {
            updates.status = 'LOST';
            autoLostReason = \`\${newCallbackCount} demandes de rappel sans suite\`;
            updates.lostReason = autoLostReason;
        } else {
            updates.status = 'CONTACTED';
        }

        await SalesStore.updateProspect(selectedProspect.id, updates);

        const outcomeLabels: Record<string, string> = {
            'NO_ANSWER': 'Pas de réponse',
            'CALLBACK': 'À rappeler',
            'NOT_INTERESTED': 'Pas intéressé',
            'WRONG_NUMBER': 'Mauvais numéro',
            'INTERESTED': 'Intéressé — Qualifié ✅'
        };
        let noteText = \`📞 Tentative #\${newCallAttempts} — \${outcomeLabels[outcome]}\`;
        if (outcome === 'NO_ANSWER') noteText += \` (\${newNoAnswerCount}/\${MAX_NO_ANSWER})\`;
        if (outcome === 'CALLBACK') noteText += \` (\${newCallbackCount}/\${MAX_CALLBACKS})\`;
        if (autoLostReason) noteText += \`\n⚠️ Auto-classé PERDU : \${autoLostReason}\`;

        await SalesStore.addNote(selectedProspect.id, noteText);

        if (autoLostReason) {
            showToast(\`Prospect automatiquement classé PERDU — \${autoLostReason}\`, 'warning', '⚠️');
        } else {
            const outcomeEmoji: Record<string, string> = { 'NO_ANSWER': '📵', 'CALLBACK': '🔄', 'NOT_INTERESTED': '❌', 'WRONG_NUMBER': '⚠️', 'INTERESTED': '✅' };
            showToast(\`\${outcomeLabels[outcome]} enregistré (tentative #\${newCallAttempts})\`, 'info', outcomeEmoji[outcome] || '📞');
        }

        loadProspects();
    };

    return (
        <>
            ${drawerCode.replace(/selectedProspect && \(/, '').replace(/export function PaymentButtons(.*?)function ServicePriceDisplay/s, '') /* basic cleanup if needed, but we keep drawerCode as is */}
        </>
    );
}
`;

fs.writeFileSync(path.join(__dirname, 'components/sales/ProspectDrawer.tsx'), prospectDrawerContent);

// Replace in SalesDashboard.tsx
lines.splice(startIdx, endIdx - startIdx + 1, `                    {/* Drawer Detail — Fiche Prospect Enrichie */}
                    <ProspectDrawer 
                        selectedProspect={selectedProspect} 
                        setSelectedProspect={setSelectedProspect}
                        setProspects={setProspects}
                        loadProspects={loadProspects}
                        COLUMNS={COLUMNS}
                        setShowCallCockpit={setShowCallCockpit}
                        setShowBookingModal={setShowBookingModal}
                        setShowSimulatorModal={setShowSimulatorModal}
                        setShowFollowUpModal={setShowFollowUpModal}
                        setShowTagsModal={setShowTagsModal}
                        handleStatusChange={handleStatusChange}
                        handleNoShow={handleNoShow}
                        handleStartPayment={handleStartPayment}
                        fetchServicePrice={fetchServicePrice}
                        handleCancelAppointment={handleCancelAppointment}
                        handleMarkAsLost={handleMarkAsLost}
                        handleDeleteProspect={handleDeleteProspect}
                        loadTimeline={loadTimeline}
                        setConfirmDialog={setConfirmDialog}
                        showToast={showToast}
                        callHistory={callHistory}
                    />`);

fs.writeFileSync(filePath, lines.join('\n'));
console.log('Replaced successfully');
