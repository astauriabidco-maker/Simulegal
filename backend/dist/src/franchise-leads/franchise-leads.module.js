"use strict";
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.FranchiseLeadsModule = void 0;
const common_1 = require("@nestjs/common");
const franchise_leads_service_1 = require("./franchise-leads.service");
const franchise_leads_controller_1 = require("./franchise-leads.controller");
const prisma_module_1 = require("../prisma/prisma.module");
const agencies_module_1 = require("../agencies/agencies.module");
const users_module_1 = require("../users/users.module");
const devices_module_1 = require("../devices/devices.module");
let FranchiseLeadsModule = class FranchiseLeadsModule {
};
exports.FranchiseLeadsModule = FranchiseLeadsModule;
exports.FranchiseLeadsModule = FranchiseLeadsModule = __decorate([
    (0, common_1.Module)({
        imports: [prisma_module_1.PrismaModule, agencies_module_1.AgenciesModule, users_module_1.UsersModule, devices_module_1.DevicesModule],
        providers: [franchise_leads_service_1.FranchiseLeadsService],
        controllers: [franchise_leads_controller_1.FranchiseLeadsController]
    })
], FranchiseLeadsModule);
//# sourceMappingURL=franchise-leads.module.js.map