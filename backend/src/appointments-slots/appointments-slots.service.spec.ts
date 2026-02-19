import { Test, TestingModule } from '@nestjs/testing';
import { AppointmentsSlotsService } from './appointments-slots.service';

describe('AppointmentsSlotsService', () => {
  let service: AppointmentsSlotsService;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      providers: [AppointmentsSlotsService],
    }).compile();

    service = module.get<AppointmentsSlotsService>(AppointmentsSlotsService);
  });

  it('should be defined', () => {
    expect(service).toBeDefined();
  });
});
