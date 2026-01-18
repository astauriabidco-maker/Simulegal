import { Controller, Get, Param } from '@nestjs/common';
import { EligibilityService } from './eligibility.service';

@Controller('eligibility')
export class EligibilityController {
    constructor(private readonly service: EligibilityService) { }

    @Get('thresholds')
    getThresholds() {
        return this.service.getThresholds();
    }

    @Get('rules/:category')
    getRules(@Param('category') category: string) {
        return this.service.getRules(category);
    }
}
