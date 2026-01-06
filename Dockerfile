FROM python:3.9-slim

WORKDIR /app

# Install system dependencies for some python packages (like fonts/rendering)
RUN apt-get update && apt-get install -y \
    build-essential \
    libpq-dev \
    && rm -rf /var/lib/apt/lists/*

COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt

COPY . .

# Expose the port
EXPOSE 8000

# Start command is in docker-compose
