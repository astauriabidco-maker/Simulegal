"use strict";
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
var __metadata = (this && this.__metadata) || function (k, v) {
    if (typeof Reflect === "object" && typeof Reflect.metadata === "function") return Reflect.metadata(k, v);
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.AssignmentService = void 0;
const common_1 = require("@nestjs/common");
const prisma_service_1 = require("../prisma/prisma.service");
let AssignmentService = class AssignmentService {
    prisma;
    lastAssignedIndex = new Map();
    constructor(prisma) {
        this.prisma = prisma;
    }
    async getNextSalesAgent(agencyId) {
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
        const key = agencyId || 'HQ';
        const lastIndex = this.lastAssignedIndex.get(key) ?? -1;
        const nextIndex = (lastIndex + 1) % agents.length;
        this.lastAssignedIndex.set(key, nextIndex);
        const selectedAgent = agents[nextIndex];
        console.log(`[Assignment] Assigned to ${selectedAgent.name} (${selectedAgent.id}) - Index ${nextIndex}/${agents.length}`);
        return selectedAgent.id;
    }
    resetForAgency(agencyId) {
        const key = agencyId || 'HQ';
        this.lastAssignedIndex.delete(key);
    }
    getStats() {
        return new Map(this.lastAssignedIndex);
    }
};
exports.AssignmentService = AssignmentService;
exports.AssignmentService = AssignmentService = __decorate([
    (0, common_1.Injectable)(),
    __metadata("design:paramtypes", [prisma_service_1.PrismaService])
], AssignmentService);
//# sourceMappingURL=assignment.service.js.map