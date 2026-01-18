import { Injectable } from '@nestjs/common';
import { PrismaService } from '../prisma/prisma.service';

@Injectable()
export class DevicesService {
    constructor(private prisma: PrismaService) { }

    /**
     * Generate a unique pairing code (format: ABCD-1234)
     */
    private generatePairingCode(): string {
        const letters = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
        const numbers = '0123456789';
        let code = '';
        for (let i = 0; i < 4; i++) code += letters.charAt(Math.floor(Math.random() * letters.length));
        code += '-';
        for (let i = 0; i < 4; i++) code += numbers.charAt(Math.floor(Math.random() * numbers.length));
        return code;
    }

    /**
     * Register a new device (called by tablet on first launch)
     */
    async register() {
        const device = await this.prisma.device.create({
            data: {
                pairingCode: this.generatePairingCode(),
                status: 'UNPAIRED',
                name: 'Nouveau Terminal',
                appVersion: '1.0.0',
                lastHeartbeat: new Date()
            }
        });

        console.log(`[DEVICES] 📱 New device registered: ${device.id} (code: ${device.pairingCode})`);
        return device;
    }

    /**
     * Create a pre-provisioned device for a specific agency (e.g. for Corners)
     */
    async createProvisioned(agencyId: string, agencyName: string) {
        const device = await this.prisma.device.create({
            data: {
                pairingCode: this.generatePairingCode(),
                status: 'UNPAIRED',
                name: `Borne ${agencyName}`,
                assignedAgencyId: agencyId,
                appVersion: '1.0.0',
                lastHeartbeat: new Date()
            }
        });

        console.log(`[DEVICES] 🤖 Auto-provisioned device for agency ${agencyId}: ${device.id}`);
        return device;
    }

    /**
     * Activate a device using its pairing code (called by tablet)
     */
    async activate(code: string) {
        const device = await this.prisma.device.findUnique({
            where: { pairingCode: code.toUpperCase() }
        });

        if (!device) {
            return null;
        }

        const updated = await this.prisma.device.update({
            where: { id: device.id },
            data: {
                status: 'ACTIVE',
                lastHeartbeat: new Date()
            },
            include: { assignedAgency: true }
        });

        console.log(`[DEVICES] 🚀 Device activated via code: ${updated.id}`);
        return updated;
    }

    /**
     * Get all devices
     */
    async findAll() {
        return this.prisma.device.findMany({
            include: { assignedAgency: true },
            orderBy: { createdAt: 'desc' }
        });
    }

    /**
     * Get a single device by ID
     */
    async findById(id: string) {
        return this.prisma.device.findUnique({
            where: { id },
            include: { assignedAgency: true }
        });
    }

    /**
     * Get a device by pairing code
     */
    async findByCode(code: string) {
        return this.prisma.device.findUnique({
            where: { pairingCode: code.toUpperCase() },
            include: { assignedAgency: true }
        });
    }

    /**
     * Pair a device to an agency (called by HQ admin)
     */
    async pair(pairingCode: string, agencyId: string, name?: string) {
        const device = await this.prisma.device.findUnique({
            where: { pairingCode: pairingCode.toUpperCase() }
        });

        if (!device) {
            return null;
        }

        const updated = await this.prisma.device.update({
            where: { id: device.id },
            data: {
                status: 'ACTIVE',
                assignedAgencyId: agencyId,
                name: name || device.name,
                lastHeartbeat: new Date()
            },
            include: { assignedAgency: true }
        });

        console.log(`[DEVICES] ✅ Device paired: ${device.id} → ${agencyId} `);
        return updated;
    }

    /**
     * Update device heartbeat (called by tablet periodically)
     */
    async heartbeat(id: string) {
        try {
            const device = await this.prisma.device.update({
                where: { id },
                data: {
                    lastHeartbeat: new Date(),
                    status: 'ACTIVE'
                }
            });
            return device;
        } catch {
            return null;
        }
    }

    /**
     * Reset a device (for re-pairing)
     */
    async reset(id: string) {
        return this.prisma.device.update({
            where: { id },
            data: {
                status: 'UNPAIRED',
                pairingCode: this.generatePairingCode(),
                assignedAgencyId: null
            }
        });
    }

    /**
     * Delete a device
     */
    async remove(id: string) {
        return this.prisma.device.delete({
            where: { id }
        });
    }

    /**
     * Get devices for a specific agency
     */
    async findByAgency(agencyId: string) {
        return this.prisma.device.findMany({
            where: { assignedAgencyId: agencyId },
            include: { assignedAgency: true }
        });
    }
}
