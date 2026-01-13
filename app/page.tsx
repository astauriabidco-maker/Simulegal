'use client';

import React, { useState } from 'react';
import SimulatorWrapper from '@/components/SimulatorWrapper';
import LandingPage from '@/components/LandingPage';

export default function Home() {
  const [showSimulator, setShowSimulator] = useState(false);

  const handleStartSimulator = (serviceId?: string) => {
    // Note: We could use serviceId to pre-configure the simulator state here if needed
    console.log(`Starting simulator for service: ${serviceId || 'Generic'}`);
    setShowSimulator(true);
  };

  if (showSimulator) {
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
        <SimulatorWrapper />
      </main>
    );
  }

  return <LandingPage onStartSimulator={handleStartSimulator} />;
}
