const fs = require('fs');
const path = require('path');

const filePath = path.join(__dirname, 'components/sales/SalesDashboard.tsx');
let content = fs.readFileSync(filePath, 'utf-8');
const lines = content.split('\n');

const startLine = 1194; // 1-indexed
const endLine = 2167; // 1-indexed

const drawerCodeLines = lines.slice(startLine - 1, endLine);
const drawerCode = drawerCodeLines.join('\n');

const prospectDrawerContent = `import React, { useState, useEffect } from 'react';
import { useRouter } from 'next/navigation';
import { 
    XCircle, Phone, Mail, MapPin, CheckCircle, Calendar, Microscope, Flame, ArrowRight 
} from 'lucide-react';
import { SalesStore } from '../../services/SalesStore';
import { Prospect, ProspectNote } from '../../services/SalesStore';
import { WhatsAppWidget } from '../backoffice/WhatsAppWidget';
import { SERVICES_CATALOG } from '../../data/services';
import { PaymentButtons, ServicePriceDisplay } from './SalesShared';

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
${drawerCode.replace('                    {/* Drawer Detail — Fiche Prospect Enrichie */}', '')}
        </>
    );
}
`;

fs.writeFileSync(path.join(__dirname, 'components/sales/ProspectDrawer.tsx'), prospectDrawerContent);

// Modify SalesDashboard.tsx
lines.splice(startLine - 1, drawerCodeLines.length, `                    {/* Drawer Detail — Fiche Prospect Enrichie */}
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
console.log('Done mapping lines 1194 to 2167 into ProspectDrawer.tsx!');
