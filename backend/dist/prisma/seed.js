"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || (function () {
    var ownKeys = function(o) {
        ownKeys = Object.getOwnPropertyNames || function (o) {
            var ar = [];
            for (var k in o) if (Object.prototype.hasOwnProperty.call(o, k)) ar[ar.length] = k;
            return ar;
        };
        return ownKeys(o);
    };
    return function (mod) {
        if (mod && mod.__esModule) return mod;
        var result = {};
        if (mod != null) for (var k = ownKeys(mod), i = 0; i < k.length; i++) if (k[i] !== "default") __createBinding(result, mod, k[i]);
        __setModuleDefault(result, mod);
        return result;
    };
})();
Object.defineProperty(exports, "__esModule", { value: true });
const client_1 = require("@prisma/client");
const bcrypt = __importStar(require("bcrypt"));
const prisma = new client_1.PrismaClient();
async function main() {
    await prisma.payout.deleteMany({});
    await prisma.leadNote.deleteMany({});
    await prisma.lead.deleteMany({});
    await prisma.user.deleteMany({});
    await prisma.agency.deleteMany({});
    const hashedPassword = await bcrypt.hash('demo', 10);
    const superAdminPassword = await bcrypt.hash('demo', 10);
    const hq = await prisma.agency.create({
        data: {
            id: 'HQ-001',
            name: 'SimuLegal HQ (Paris)',
            type: 'PHYSICAL',
            status: 'ACTIVE',
            zipCodes: '75001, 75002, 75003, 75004, 75005, 75006, 75007, 75008, 75009, 75010',
            commissionRate: 0,
            contactEmail: 'contact@hq.simulegal.fr',
            kioskUrl: 'https://app.simulegal.fr/?ref=HQ-001'
        }
    });
    const lyon = await prisma.agency.create({
        data: {
            id: 'OWN-001',
            name: 'SimuLegal Direct Lyon',
            type: 'PHYSICAL',
            status: 'ACTIVE',
            zipCodes: '69001, 69002, 69003, 69004, 69005, 69006, 69007, 69008, 69009',
            commissionRate: 0,
            contactEmail: 'contact@lyon.simulegal.fr',
            kioskUrl: 'https://app.simulegal.fr/?ref=OWN-001'
        }
    });
    const marseille = await prisma.agency.create({
        data: {
            id: 'FRAN-001',
            name: 'Franchise Marseille',
            type: 'PHYSICAL',
            status: 'ACTIVE',
            zipCodes: '13001, 13002, 13003, 13004, 13005, 13006, 13007, 13008',
            commissionRate: 15,
            contactEmail: 'contact@marseille.simulegal.fr',
            kioskUrl: 'https://app.simulegal.fr/?ref=FRAN-001'
        }
    });
    const bordeaux = await prisma.agency.create({
        data: {
            id: 'RELAY-001',
            name: 'Point Relais - Tabac Presse Bordeaux',
            type: 'CORNER',
            status: 'ACTIVE',
            zipCodes: '33000, 33100, 33200, 33300, 33800',
            commissionRate: 10,
            contactEmail: 'contact@bordeaux.simulegal.fr',
            kioskUrl: 'https://app.simulegal.fr/?ref=RELAY-001'
        }
    });
    await prisma.user.createMany({
        data: [
            {
                email: 'hq.admin@simulegal.fr',
                password: hashedPassword,
                name: 'Sophie Martin (Siège)',
                role: 'HQ_ADMIN',
                permissions: 'view_all_leads,validate_documents,manage_agencies,view_reports',
                expertises: '["cs_salarie", "cs_etudiant"]'
            },
            {
                email: 'juridique@simulegal.fr',
                password: hashedPassword,
                name: 'Marie Dupont',
                role: 'HQ_ADMIN',
                permissions: 'view_all_leads,validate_documents',
                expertises: '["naturalisation", "permis_conduire"]'
            },
            {
                email: 'agency.paris@simulegal.fr',
                password: hashedPassword,
                name: 'Agence Paris Louvre',
                role: 'AGENCY_MANAGER',
                agencyId: 'HQ-001',
                permissions: 'view_own_leads,add_notes'
            },
            {
                email: 'agence.lyon@simulegal.fr',
                password: hashedPassword,
                name: 'Sophie Bernard',
                role: 'AGENCY_MANAGER',
                agencyId: 'OWN-001',
                permissions: 'view_own_leads,add_notes',
                expertises: '["naturalisation"]'
            },
            {
                email: 'relay.bordeaux@simulegal.fr',
                password: hashedPassword,
                name: 'Jean Relais',
                role: 'KIOSK_AGENT',
                agencyId: 'RELAY-001',
                permissions: 'view_own_leads'
            },
            {
                email: 'super.admin@simulegal.fr',
                password: superAdminPassword,
                name: 'Admin Système',
                role: 'SUPER_ADMIN',
                permissions: '*'
            }
        ]
    });
    const demoLeads = [
        {
            id: 'DEMO-NAT-001',
            name: 'Jean Démo',
            email: 'candidat@demo.fr',
            phone: '0601020304',
            serviceId: 'naturalisation',
            serviceName: 'Naturalisation Française',
            status: 'PAID',
            amountPaid: 4900,
            originAgency: { connect: { id: 'HQ-001' } }
        },
        {
            id: 'DEMO-VPF-001',
            name: 'Marie Séjour',
            email: 'marie@demo.fr',
            phone: '0601020305',
            serviceId: 'vpf_conjoint_francais',
            serviceName: 'Titre de Séjour (Conjoint Français)',
            status: 'PAID',
            amountPaid: 3500,
            originAgency: { connect: { id: 'HQ-001' } }
        },
        {
            id: 'DEMO-SAL-001',
            name: 'Paul Salarié',
            email: 'paul@demo.fr',
            phone: '0601020306',
            serviceId: 'cs_salarie',
            serviceName: 'Titre de Séjour (Salarié)',
            status: 'PAID',
            amountPaid: 3500,
            originAgency: { connect: { id: 'HQ-001' } }
        },
        {
            id: 'DEMO-ETU-001',
            name: 'Léa Étudiante',
            email: 'lea@demo.fr',
            phone: '0601020307',
            serviceId: 'cs_etudiant',
            serviceName: 'Titre de Séjour (Étudiant)',
            status: 'PAID',
            amountPaid: 1500,
            originAgency: { connect: { id: 'HQ-001' } }
        },
        {
            id: 'DEMO-TALENT-001',
            name: 'Alex Talent',
            email: 'alex@demo.fr',
            phone: '0601020308',
            serviceId: 'passeport_talent_carte_bleue_eu',
            serviceName: 'Passeport Talent',
            status: 'PAID',
            amountPaid: 5900,
            originAgency: { connect: { id: 'HQ-001' } }
        },
        {
            id: 'DEMO-PERMIS-001',
            name: 'Marc Permis',
            email: 'marc@demo.fr',
            phone: '0601020309',
            serviceId: 'permis_conduire',
            serviceName: 'Échange de Permis',
            status: 'PAID',
            amountPaid: 900,
            originAgency: { connect: { id: 'HQ-001' } }
        }
    ];
    for (const lead of demoLeads) {
        await prisma.lead.create({ data: lead });
    }
    console.log('Seed completed! 🚀');
}
main()
    .catch((e) => {
    console.error(e);
    process.exit(1);
})
    .finally(async () => {
    await prisma.$disconnect();
});
//# sourceMappingURL=seed.js.map