"use strict";
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.EligibilityService = void 0;
const common_1 = require("@nestjs/common");
let EligibilityService = class EligibilityService {
    getThresholds() {
        return {
            "smic_mensuel_brut": 1766.92,
            "smic_mensuel_net": 1398.69,
            "smic_annuel_brut": 21203.00,
            "min_residence_naturalisation_years": 5,
            "min_residence_conjoint_years": 4,
            "min_residence_etudiant_reduc_years": 2,
            "logement_surface_base": 9,
            "logement_surface_per_person": 9,
            "frais_timbre_naturalisation": 55,
            "frais_timbre_sejour_base": 225
        };
    }
    getRules(category) {
        return [];
    }
};
exports.EligibilityService = EligibilityService;
exports.EligibilityService = EligibilityService = __decorate([
    (0, common_1.Injectable)()
], EligibilityService);
//# sourceMappingURL=eligibility.service.js.map