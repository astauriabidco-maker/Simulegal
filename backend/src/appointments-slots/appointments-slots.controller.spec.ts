import { Test, TestingModule } from '@nestjs/testing';
import { AppointmentsSlotsController } from './appointments-slots.controller';

describe('AppointmentsSlotsController', () => {
  let controller: AppointmentsSlotsController;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      controllers: [AppointmentsSlotsController],
    }).compile();

    controller = module.get<AppointmentsSlotsController>(AppointmentsSlotsController);
  });

  it('should be defined', () => {
    expect(controller).toBeDefined();
  });
});
