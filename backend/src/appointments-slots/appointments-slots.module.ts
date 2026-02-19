import { Module } from '@nestjs/common';
import { PrismaModule } from '../prisma/prisma.module';
import { AppointmentsSlotsController } from './appointments-slots.controller';
import { AppointmentsSlotsService } from './appointments-slots.service';

@Module({
  imports: [PrismaModule],
  controllers: [AppointmentsSlotsController],
  providers: [AppointmentsSlotsService]
})
export class AppointmentsSlotsModule { }
