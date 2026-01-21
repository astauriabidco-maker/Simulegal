import { PrismaService } from '../prisma/prisma.service';
export declare class AssignmentService {
    private prisma;
    private lastAssignedIndex;
    constructor(prisma: PrismaService);
    getNextSalesAgent(agencyId: string | null): Promise<string | null>;
    resetForAgency(agencyId: string | null): void;
    getStats(): Map<string, number>;
}
