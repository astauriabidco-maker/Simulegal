import { Role } from '@prisma/client';

export class CreateUserDto {
    email: string;
    firstName?: string;
    lastName?: string;
    role?: Role;
}
