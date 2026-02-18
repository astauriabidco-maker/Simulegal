import { Module } from '@nestjs/common';
import { EligibilityController } from './eligibility.controller';
import { EligibilityService } from './eligibility.service';
import { PrismaModule } from '../prisma/prisma.module';

@Module({
    imports: [PrismaModule],
    controllers: [EligibilityController],
    providers: [EligibilityService],
    exports: [EligibilityService],
})
export class EligibilityModule { }
