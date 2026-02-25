import { create } from 'zustand';

interface PaymentState {
    loading: boolean;
    error: string | null;
    createCheckoutSession: (leadId: string) => Promise<void>;
}

export const usePaymentStore = create<PaymentState>((set) => ({
    loading: false,
    error: null,

    createCheckoutSession: async (leadId: string) => {
        set({ loading: true, error: null });
        try {
            const API_URL = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:4000';

            // Get current host to build return URLs
            const host = window.location.origin;

            const response = await fetch(`${API_URL}/payments/create-session`, {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json',
                    // Auth header would go here if needed, 
                    // though B2C users might not have a JWT yet
                    // 'Authorization': `Bearer ${localStorage.getItem('token')}`
                },
                body: JSON.stringify({
                    leadId,
                    successUrl: `${host}/payment/success?session_id={CHECKOUT_SESSION_ID}`,
                    cancelUrl: `${host}/payment/cancel`,
                }),
            });

            if (!response.ok) {
                throw new Error('Failed to create checkout session');
            }

            const { url } = await response.json();

            // Redirect to Stripe
            if (url) {
                window.location.href = url;
            } else {
                throw new Error('No checkout URL received');
            }

        } catch (error: any) {
            console.error('[Payment] Session Error:', error);
            set({ error: error.message || 'Error occurred during payment initialization', loading: false });
        }
    },
}));
