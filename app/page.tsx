'use client';

import React, { useState, useEffect } from 'react';
import SimulatorWrapper from '@/components/SimulatorWrapper';
import LandingPage from '@/components/LandingPage';
import CheckoutFlow from '@/components/CheckoutFlow';

declare global {
  interface Window {
    openAuthAndPaymentFlow: (serviceId: string, price: number, name?: string) => void;
  }
}

export default function Home() {
  const [showSimulator, setShowSimulator] = useState(false);
  const [selectedService, setSelectedService] = useState<string | undefined>(undefined);

  // Checkout State
  const [isCheckoutOpen, setIsCheckoutOpen] = useState(false);
  const [checkoutConfig, setCheckoutConfig] = useState({ serviceId: '', serviceName: '', price: 0 });

  useEffect(() => {
    window.openAuthAndPaymentFlow = (serviceId: string, price: number, name?: string) => {
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
        price
      });
      setIsCheckoutOpen(true);
    };
  }, []);

  const handleStartSimulator = (serviceId?: string) => {
    console.log(`Starting simulator for service: ${serviceId || 'Generic'}`);
    setSelectedService(serviceId);
    setShowSimulator(true);
  };

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
          <SimulatorWrapper serviceId={selectedService} />
        </main>
      ) : (
        <LandingPage onStartSimulator={handleStartSimulator} />
      )}

      <CheckoutFlow
        isOpen={isCheckoutOpen}
        onClose={() => setIsCheckoutOpen(false)}
        {...checkoutConfig}
      />
    </div>
  );
}
