'use client';

import React, { useState } from 'react';
import { Calendar, MapPin, Clock, X, Send, CheckCircle } from 'lucide-react';
import { AppointmentInfo } from '../../services/SalesStore';

interface BookAppointmentModalProps {
    prospectName: string;
    prospectPhone: string;
    defaultServiceId?: string;
    existingAppointment?: AppointmentInfo;
    onBook: (appointment: AppointmentInfo) => void;
    onClose: () => void;
}

// Agences disponibles (à terme, chargées depuis le backend)
const AGENCIES = [
    { id: 'AGC-PARIS-15', name: 'Agence Paris 15ème', address: '45 rue de Vouillé, 75015 Paris' },
    { id: 'AGC-PARIS-11', name: 'Agence Paris 11ème', address: '12 boulevard Voltaire, 75011 Paris' },
    { id: 'AGC-LYON', name: 'Agence Lyon Part-Dieu', address: '8 place Charles Béraudier, 69003 Lyon' },
    { id: 'AGC-MARSEILLE', name: 'Agence Marseille Centre', address: '15 La Canebière, 13001 Marseille' },
    { id: 'AGC-BORDEAUX', name: 'Agence Bordeaux', address: '20 cours de l\'Intendance, 33000 Bordeaux' },
];

export default function BookAppointmentModal({
    prospectName,
    prospectPhone,
    defaultServiceId,
    existingAppointment,
    onBook,
    onClose
}: BookAppointmentModalProps) {
    const [date, setDate] = useState(
        existingAppointment?.date
            ? new Date(existingAppointment.date).toISOString().slice(0, 16)
            : ''
    );
    const [agencyId, setAgencyId] = useState(existingAppointment?.agencyId || '');
    const [sendConfirmation, setSendConfirmation] = useState(true);
    const [confirmationChannel, setConfirmationChannel] = useState<'SMS' | 'WHATSAPP'>('SMS');
    const [isSubmitting, setIsSubmitting] = useState(false);

    const selectedAgency = AGENCIES.find(a => a.id === agencyId);

    const handleSubmit = async () => {
        if (!date || !agencyId) {
            alert('Veuillez remplir la date et l\'agence.');
            return;
        }

        setIsSubmitting(true);

        const appointment: AppointmentInfo = {
            date: new Date(date).toISOString(),
            agencyId,
            agencyName: selectedAgency?.name || agencyId,
            serviceId: defaultServiceId,
            confirmed: sendConfirmation,
            confirmationSentVia: sendConfirmation ? confirmationChannel : undefined,
        };

        onBook(appointment);
        setIsSubmitting(false);
    };

    // Générer les créneaux disponibles (prochains 14 jours, 9h-18h)
    const minDate = new Date();
    minDate.setHours(minDate.getHours() + 1);
    const minDateStr = minDate.toISOString().slice(0, 16);

    return (
        <div className="fixed inset-0 z-[60] flex items-center justify-center">
            {/* Backdrop */}
            <div className="absolute inset-0 bg-black/40 backdrop-blur-sm" onClick={onClose} />

            {/* Modal */}
            <div className="relative w-[480px] bg-white rounded-3xl shadow-2xl overflow-hidden animate-in zoom-in-95 duration-200">
                {/* Header */}
                <div className="relative bg-gradient-to-br from-indigo-600 via-indigo-700 to-purple-700 px-6 py-5">
                    <button
                        onClick={onClose}
                        className="absolute top-4 right-4 p-1.5 hover:bg-white/10 rounded-full text-white/60 hover:text-white transition-colors"
                    >
                        <X size={18} />
                    </button>
                    <div className="flex items-center gap-3">
                        <div className="w-11 h-11 rounded-xl bg-white/15 flex items-center justify-center">
                            <Calendar size={22} className="text-white" />
                        </div>
                        <div>
                            <h2 className="text-lg font-bold text-white">
                                {existingAppointment ? 'Modifier le RDV' : 'Fixer un RDV en agence'}
                            </h2>
                            <p className="text-indigo-200 text-sm">{prospectName} • {prospectPhone}</p>
                        </div>
                    </div>
                </div>

                {/* Form */}
                <div className="p-6 space-y-5">
                    {/* Date / Heure */}
                    <div>
                        <label className="block text-xs font-bold text-slate-500 uppercase tracking-wider mb-2">
                            <Clock size={13} className="inline mr-1.5 -mt-0.5" />
                            Date et heure du RDV
                        </label>
                        <input
                            type="datetime-local"
                            value={date}
                            onChange={(e) => setDate(e.target.value)}
                            min={minDateStr}
                            className="w-full px-4 py-3 border border-slate-200 rounded-xl text-slate-900 text-sm font-medium focus:border-indigo-400 focus:ring-2 focus:ring-indigo-100 outline-none transition-all"
                        />
                    </div>

                    {/* Agence */}
                    <div>
                        <label className="block text-xs font-bold text-slate-500 uppercase tracking-wider mb-2">
                            <MapPin size={13} className="inline mr-1.5 -mt-0.5" />
                            Agence
                        </label>
                        <div className="space-y-2">
                            {AGENCIES.map((agency) => (
                                <button
                                    key={agency.id}
                                    onClick={() => setAgencyId(agency.id)}
                                    className={`w-full text-left px-4 py-3 rounded-xl border-2 transition-all ${agencyId === agency.id
                                            ? 'border-indigo-500 bg-indigo-50 shadow-sm'
                                            : 'border-slate-100 bg-white hover:border-slate-200'
                                        }`}
                                >
                                    <p className={`text-sm font-bold ${agencyId === agency.id ? 'text-indigo-700' : 'text-slate-900'}`}>
                                        {agency.name}
                                    </p>
                                    <p className="text-xs text-slate-400 mt-0.5">{agency.address}</p>
                                </button>
                            ))}
                        </div>
                    </div>

                    {/* Confirmation */}
                    <div className="bg-slate-50 rounded-xl p-4 border border-slate-100">
                        <div className="flex items-center justify-between mb-3">
                            <label className="text-sm font-bold text-slate-700 flex items-center gap-2">
                                <Send size={14} />
                                Envoyer confirmation au lead
                            </label>
                            <button
                                onClick={() => setSendConfirmation(!sendConfirmation)}
                                className={`relative w-11 h-6 rounded-full transition-colors ${sendConfirmation ? 'bg-indigo-600' : 'bg-slate-300'}`}
                            >
                                <div className={`absolute top-0.5 w-5 h-5 bg-white rounded-full shadow transition-transform ${sendConfirmation ? 'translate-x-5' : 'translate-x-0.5'}`} />
                            </button>
                        </div>
                        {sendConfirmation && (
                            <div className="flex gap-2">
                                <button
                                    onClick={() => setConfirmationChannel('SMS')}
                                    className={`flex-1 py-2 rounded-lg text-xs font-bold transition-all ${confirmationChannel === 'SMS'
                                            ? 'bg-indigo-600 text-white'
                                            : 'bg-white border border-slate-200 text-slate-500 hover:border-slate-300'
                                        }`}
                                >
                                    📱 SMS
                                </button>
                                <button
                                    onClick={() => setConfirmationChannel('WHATSAPP')}
                                    className={`flex-1 py-2 rounded-lg text-xs font-bold transition-all ${confirmationChannel === 'WHATSAPP'
                                            ? 'bg-emerald-600 text-white'
                                            : 'bg-white border border-slate-200 text-slate-500 hover:border-slate-300'
                                        }`}
                                >
                                    💬 WhatsApp
                                </button>
                            </div>
                        )}
                    </div>

                    {/* Preview */}
                    {date && selectedAgency && (
                        <div className="bg-indigo-50 rounded-xl p-4 border border-indigo-100">
                            <p className="text-xs font-black text-indigo-600 uppercase tracking-wider mb-2">Récapitulatif</p>
                            <div className="space-y-1.5 text-sm text-slate-700">
                                <p>📍 <strong>{selectedAgency.name}</strong></p>
                                <p>🗓 {new Date(date).toLocaleDateString('fr-FR', { weekday: 'long', day: 'numeric', month: 'long', year: 'numeric' })}</p>
                                <p>🕐 {new Date(date).toLocaleTimeString('fr-FR', { hour: '2-digit', minute: '2-digit' })}</p>
                                {defaultServiceId && <p>🎯 Service : {defaultServiceId}</p>}
                            </div>
                        </div>
                    )}
                </div>

                {/* Footer */}
                <div className="px-6 py-4 bg-slate-50 border-t border-slate-100 flex gap-3">
                    <button
                        onClick={onClose}
                        className="flex-1 py-3 rounded-xl border border-slate-200 text-slate-600 font-bold text-sm hover:bg-white transition-all"
                    >
                        Annuler
                    </button>
                    <button
                        onClick={handleSubmit}
                        disabled={!date || !agencyId || isSubmitting}
                        className="flex-1 py-3 rounded-xl bg-indigo-600 text-white font-bold text-sm hover:bg-indigo-700 transition-all shadow-sm shadow-indigo-200 disabled:opacity-50 disabled:cursor-not-allowed flex items-center justify-center gap-2"
                    >
                        {isSubmitting ? (
                            <span className="animate-spin">⏳</span>
                        ) : (
                            <>
                                <CheckCircle size={16} />
                                {existingAppointment ? 'Modifier' : 'Confirmer le RDV'}
                            </>
                        )}
                    </button>
                </div>
            </div>
        </div>
    );
}
