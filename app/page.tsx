'use client';

import React, { useState, useEffect } from 'react';
import SimulatorWrapper from '@/components/SimulatorWrapper';
import LandingPage from '@/components/LandingPage';
import CheckoutFlow from '@/components/CheckoutFlow';
import HQDashboard from '@/components/backoffice/HQDashboard';
import AgencyDashboard from '@/components/backoffice/AgencyDashboard';
import { Building2, LayoutDashboard, X, Radio, Shield } from 'lucide-react';

declare global {
  interface Window {
    openAuthAndPaymentFlow: (serviceId: string, price: number, name?: string, forceAgencyId?: string, extraData?: any) => void;
    kioskPartnerId: string | null;
  }
}

type DevView = 'none' | 'hq' | 'agency';

export default function Home() {
  const [showSimulator, setShowSimulator] = useState(false);
  const [selectedService, setSelectedService] = useState<string | undefined>(undefined);
  const [prospectId, setProspectId] = useState<string | undefined>(undefined);
  const [devView, setDevView] = useState<DevView>('none');
  const [partnerId, setPartnerId] = useState<string | null>(null);

  // Checkout State
  const [isCheckoutOpen, setIsCheckoutOpen] = useState(false);
  const [checkoutConfig, setCheckoutConfig] = useState({ serviceId: '', serviceName: '', price: 0, extraData: null });

  // Capture du code partenaire depuis l'URL
  useEffect(() => {
    // Récupère depuis l'URL ou le sessionStorage
    const urlParams = new URLSearchParams(window.location.search);
    const refCode = urlParams.get('ref');
    const storedRef = sessionStorage.getItem('kiosk_partner_id');

    if (refCode) {
      setPartnerId(refCode);
      sessionStorage.setItem('kiosk_partner_id', refCode);
      window.kioskPartnerId = refCode;
      console.log(`[KIOSK] Mode Borne activé - Partenaire: ${refCode}`);
    } else if (storedRef) {
      setPartnerId(storedRef);
      window.kioskPartnerId = storedRef;
    }

    const incomingProspectId = urlParams.get('prospectId');
    const incomingServiceId = urlParams.get('serviceId');
    if (incomingProspectId && incomingServiceId) {
      setProspectId(incomingProspectId);
      setSelectedService(incomingServiceId);
      setShowSimulator(true);
      // We could also try to prefill the profile from the CRM if connected to a real backend,
      // but launching it pre-filled conceptually via URL indicates the link works as intended!
    }
  }, []);

  useEffect(() => {
    window.openAuthAndPaymentFlow = (serviceId: string, price: number, name?: string, forceAgencyId?: string, extraData?: any) => {
      const names: Record<string, string> = {
        'titre_sejour': 'Accompagnement Titre de Séjour',
        'naturalisation': 'Dossier de Naturalisation',
        'rdv_juriste': 'Consultation Juridique',
        'rdv_prefecture': 'Aide au RDV Préfecture',
        'driving_exchange': 'Échange Permis Étranger'
      };

      setCheckoutConfig({
        serviceId,
        serviceName: name || names[serviceId] || 'Service Premium',
        price,
        extraData
      });
      // On injecte le code partenaire si forcé (pour dashboard agence)
      if (forceAgencyId) {
        setPartnerId(forceAgencyId);
      }
      setIsCheckoutOpen(true);
    };
  }, []);

  const handleStartSimulator = (serviceId?: string) => {
    console.log(`Starting simulator for service: ${serviceId || 'Generic'}`);
    setSelectedService(serviceId);
    setShowSimulator(true);
  };

  // Fonction de reset pour le mode Kiosk
  const handleKioskReset = () => {
    setShowSimulator(false);
    setIsCheckoutOpen(false);
    setSelectedService(undefined);
    // Garde le ref dans l'URL pour le client suivant
    const currentRef = partnerId || sessionStorage.getItem('kiosk_partner_id');
    if (currentRef) {
      window.location.href = `/?ref=${currentRef}`;
    } else {
      window.location.href = '/';
    }
  };

  // Si mode dev activé, affiche le dashboard correspondant
  if (devView !== 'none') {
    return (
      <div className="min-h-screen">
        {/* Barre de contrôle Dev */}
        <div className="fixed bottom-4 left-1/2 -translate-x-1/2 bg-slate-900 text-white px-4 py-2 rounded-full shadow-2xl flex items-center gap-3 z-50">
          <span className="text-xs font-bold text-slate-400">DEV MODE:</span>
          <button
            onClick={() => setDevView('hq')}
            className={`px-3 py-1.5 rounded-full text-xs font-bold transition-all ${devView === 'hq' ? 'bg-indigo-600 text-white' : 'bg-slate-700 text-slate-300 hover:bg-slate-600'
              }`}
          >
            Vue HQ
          </button>
          <button
            onClick={() => setDevView('agency')}
            className={`px-3 py-1.5 rounded-full text-xs font-bold transition-all ${devView === 'agency' ? 'bg-emerald-600 text-white' : 'bg-slate-700 text-slate-300 hover:bg-slate-600'
              }`}
          >
            Vue Agence
          </button>
          <button
            onClick={() => setDevView('none')}
            className="w-6 h-6 bg-red-500 rounded-full flex items-center justify-center hover:bg-red-600 transition-colors"
          >
            <X size={14} />
          </button>
        </div>

        {/* Dashboard affiché */}
        {devView === 'hq' && <HQDashboard />}
        {devView === 'agency' && <AgencyDashboard />}
      </div>
    );
  }

  return (
    <div className="min-h-screen">
      {showSimulator ? (
        <main className="min-h-screen bg-gradient-to-b from-slate-50 to-slate-100 py-12">
          <div className="max-w-4xl mx-auto px-4 mb-8 flex justify-between items-center">
            <div className="flex items-center gap-2">
              <div className="w-8 h-8 bg-indigo-600 rounded-lg flex items-center justify-center text-white font-bold">S</div>
              <span className="text-xl font-bold text-slate-900 tracking-tight">SimuLegal</span>
            </div>
            <button
              onClick={() => setShowSimulator(false)}
              className="text-sm font-medium text-slate-500 hover:text-indigo-600 transition-colors"
            >
              Retour à l'accueil
            </button>
          </div>
          <SimulatorWrapper serviceId={selectedService} prospectId={prospectId} />
        </main>
      ) : (
        <LandingPage onStartSimulator={handleStartSimulator} />
      )}

      <CheckoutFlow
        isOpen={isCheckoutOpen}
        onClose={() => setIsCheckoutOpen(false)}
        partnerId={partnerId}
        isKioskMode={!!partnerId}
        onKioskReset={handleKioskReset}
        {...checkoutConfig}
      />

      {/* KIOSK MODE: Bandeau d'indication */}
      {partnerId && (
        <div className="fixed bottom-0 left-0 right-0 bg-red-600 text-white py-2 px-4 flex items-center justify-center gap-2 z-40">
          <Radio size={16} className="animate-pulse" />
          <span className="text-sm font-bold">Mode Borne - Partenaire : {partnerId}</span>
        </div>
      )}

      {/* LIEN ACCÈS PRO SÉCURISÉ */}
      {!partnerId && (
        <div className="fixed bottom-4 right-4 z-50">
          <a
            href="/staff-login"
            className="flex items-center gap-2 px-6 py-3 bg-slate-900 hover:bg-slate-800 text-white rounded-2xl text-xs font-black transition-all shadow-2xl hover:scale-105"
          >
            <Shield size={16} className="text-indigo-400" />
            Accès Portail Pro
          </a>
        </div>
      )}
    </div>
  );
}
