const API_URL = window.location.origin;

const Auth = {
    // 1. Gestion du Token
    getToken: () => localStorage.getItem('access_token'),
    setToken: (token) => localStorage.setItem('access_token', token),
    removeToken: () => localStorage.removeItem('access_token'),

    // 2. Gestion de l'Utilisateur
    getUser: () => {
        const userStr = localStorage.getItem('user_data');
        return userStr ? JSON.parse(userStr) : null;
    },
    setUser: (user) => localStorage.setItem('user_data', JSON.stringify(user)),
    removeUser: () => localStorage.removeItem('user_data'),

    // 3. Actions
    login: async (username, password) => {
        const formData = new URLSearchParams();
        formData.append('username', username);
        formData.append('password', password);

        const response = await fetch(`${API_URL}/api/auth/token`, {
            method: 'POST',
            headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
            body: formData
        });

        if (!response.ok) {
            throw new Error('Identifiants incorrects');
        }

        const data = await response.json();
        Auth.setToken(data.access_token);

        // Récupérer les infos user
        await Auth.fetchMe();
        return true;
    },

    register: async (email, password, fullName) => {
        const response = await fetch(`${API_URL}/api/auth/register`, {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({
                email: email,
                password: password,
                full_name: fullName
            })
        });

        if (!response.ok) {
            const err = await response.json();
            throw new Error(err.detail || "Erreur lors de l'inscription");
        }

        // Connexion automatique après inscription
        return Auth.login(email, password);
    },

    fetchMe: async () => {
        const token = Auth.getToken();
        if (!token) return null;

        const response = await fetch(`${API_URL}/api/auth/me`, {
            headers: { 'Authorization': `Bearer ${token}` }
        });

        if (response.ok) {
            const user = await response.json();
            Auth.setUser(user);
            return user;
        } else {
            Auth.logout();
            return null;
        }
    },

    logout: () => {
        Auth.removeToken();
        Auth.removeUser();
        window.location.href = '/login';
    },

    isAuthenticated: () => {
        return !!Auth.getToken();
    },

    // Garde : Redirige si non connecté
    requireAuth: () => {
        if (!Auth.isAuthenticated()) {
            window.location.href = '/login';
        }
    },

    // Garde : Redirige si non admin
    requireAdmin: () => {
        const user = Auth.getUser();
        if (!user || user.role !== 'admin') {
            window.location.href = '/app'; // ou login
        }
    }
};

window.Auth = Auth;
