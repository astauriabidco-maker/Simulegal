'use client';

import React, { useState } from 'react';
import SimulatorWrapper from '@/components/SimulatorWrapper';
import LandingPage from '@/components/LandingPage';

export default function Home() {
  const [showSimulator, setShowSimulator] = useState(false);
  const [selectedService, setSelectedService] = useState<string | undefined>(undefined);

  const handleStartSimulator = (serviceId?: string) => {
    console.log(`Starting simulator for service: ${serviceId || 'Generic'}`);
    setSelectedService(serviceId);
    setShowSimulator(true);
  };

  if (showSimulator) {
    // Determine which service was selected (stored in state or passed through)
    // For now, we assume handleStartSimulator sets it (needs a state to stick)
    return (
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
    );
  }

  return <LandingPage onStartSimulator={handleStartSimulator} />;
}
