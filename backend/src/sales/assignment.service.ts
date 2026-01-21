import { Injectable } from '@nestjs/common';
import { PrismaService } from '../prisma/prisma.service';

@Injectable()
export class AssignmentService {
    // In-memory tracking of last assigned agent index per agency
    private lastAssignedIndex: Map<string, number> = new Map();

    constructor(private prisma: PrismaService) { }

    /**
     * Get the next sales agent for round-robin assignment
     * @param agencyId - The agency to find agents in (null = HQ/all agencies)
     * @returns The user ID of the next sales agent, or null if none available
     */
    async getNextSalesAgent(agencyId: string | null): Promise<string | null> {
        // Find all SALES role users for this agency
        const agents = await this.prisma.user.findMany({
            where: {
                role: 'SALES',
                ...(agencyId && { agencyId }),
            },
            orderBy: { id: 'asc' },
            select: { id: true, name: true },
        });

        if (agents.length === 0) {
            console.log(`[Assignment] No SALES agents found for agency ${agencyId || 'HQ'}`);
            return null;
        }

        // Get the last assigned index for this agency (default to -1)
        const key = agencyId || 'HQ';
        const lastIndex = this.lastAssignedIndex.get(key) ?? -1;

        // Calculate next index (round-robin)
        const nextIndex = (lastIndex + 1) % agents.length;

        // Update the tracking map
        this.lastAssignedIndex.set(key, nextIndex);

        const selectedAgent = agents[nextIndex];
        console.log(`[Assignment] Assigned to ${selectedAgent.name} (${selectedAgent.id}) - Index ${nextIndex}/${agents.length}`);

        return selectedAgent.id;
    }

    /**
     * Reset the round-robin index for an agency (useful for testing)
     */
    resetForAgency(agencyId: string | null): void {
        const key = agencyId || 'HQ';
        this.lastAssignedIndex.delete(key);
    }

    /**
     * Get current assignment stats for debugging
     */
    getStats(): Map<string, number> {
        return new Map(this.lastAssignedIndex);
    }
}
