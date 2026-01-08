import asyncio
import sys
import os

# Ensure we can import from core/api
sys.path.append(os.getcwd())

from core.database import AsyncSessionLocal
from core.auth_models import User, Agency
from api.auth import get_password_hash
from sqlalchemy import select

async def create_test_data():
    async with AsyncSessionLocal() as db:
        print("Creating Test Agency...")
        
        # 1. Create Agency
        res = await db.execute(select(Agency).where(Agency.name == "Simulegal Paris Est"))
        agency = res.scalars().first()
        
        if not agency:
            agency = Agency(
                name="Simulegal Paris Est",
                type="franchise_full",
                commission_rate=0.15,
                city="Paris"
            )
            db.add(agency)
            await db.commit()
            print(f"Agency created: {agency.name} (ID: {agency.id})")
        else:
            print(f"Agency exists: {agency.name} (ID: {agency.id})")
            
        # 2. Create Agent User
        res_user = await db.execute(select(User).where(User.email == "agent@simulegal.fr"))
        agent = res_user.scalars().first()
        
        if not agent:
            pwd_hash = get_password_hash("password")
            agent = User(
                email="agent@simulegal.fr",
                password_hash=pwd_hash,
                full_name="Jean Martin (Agent)",
                role="agent",
                agency_id=agency.id
            )
            db.add(agent)
            await db.commit()
            print("Agent created: agent@simulegal.fr / password")
        else:
            # Update role/agency if needed
            agent.role = "agent"
            agent.agency_id = agency.id
            await db.commit()
            print("Agent updated: agent@simulegal.fr")

        # 3. Create Client for this Agency
        res_client = await db.execute(select(User).where(User.email == "client.agence@test.com"))
        client = res_client.scalars().first()
        
        if not client:
            pwd_hash = get_password_hash("password")
            client = User(
                email="client.agence@test.com",
                password_hash=pwd_hash,
                full_name="Alice (Cliente Agence)",
                role="user",
                agency_id=agency.id
            )
            db.add(client)
            await db.commit()
            print("Client created: client.agence@test.com")
        
        # 4. Create Case for Client
        from core.auth_models import CaseFile
        res_case = await db.execute(select(CaseFile).where(CaseFile.user_id == client.id))
        case = res_case.scalars().first()
        
        if not case:
            case = CaseFile(
                user_id=client.id,
                status="collecting",
                submission_type="digital",
                deposit_method="upload"
            )
            db.add(case)
            await db.commit()
            print(f"Case created for client {client.email}")

        # 5. Create Admin User
        res_admin = await db.execute(select(User).where(User.email == "admin@simulegal.fr"))
        admin = res_admin.scalars().first()
        
        if not admin:
            pwd_hash = get_password_hash("admin123")
            admin = User(
                email="admin@simulegal.fr",
                password_hash=pwd_hash,
                full_name="Super Admin",
                role="admin"
            )
            db.add(admin)
            await db.commit()
            print("Admin created: admin@simulegal.fr / admin123")

if __name__ == "__main__":
    asyncio.run(create_test_data())
