import os
from sqlalchemy.ext.asyncio import create_async_engine, AsyncSession
from sqlalchemy.orm import sessionmaker, declarative_base
from dotenv import load_dotenv

load_dotenv()

# Par défaut, on utilise SQLite pour le développement local rapide ou Postgres si configuré
DATABASE_URL = os.getenv("DATABASE_URL", "sqlite+aiosqlite:///./simulegal.db")

# engine asynchrone
engine = create_async_engine(
    DATABASE_URL,
    echo=True,
)

# Fabrique de sessions asynchrones
AsyncSessionLocal = sessionmaker(
    engine, 
    class_=AsyncSession, 
    expire_on_commit=False
)

Base = declarative_base()

async def get_db():
    """Dépendance FastAPI pour obtenir une session de DB asynchrone."""
    async with AsyncSessionLocal() as session:
        yield session

async def init_db():
    """Initialise les tables (à utiliser au démarrage)."""
    async with engine.begin() as conn:
        # await conn.run_sync(Base.metadata.drop_all) # Décommenter pour reset
        await conn.run_sync(Base.metadata.create_all)
