FROM node:20-alpine AS base

# Install dependencies only when needed
FROM base AS deps
RUN apk add --no-cache libc6-compat openssl
WORKDIR /app

COPY package.json package-lock.json* ./
COPY prisma ./prisma/

RUN \
  if [ -f package-lock.json ]; then npm install; \
  else echo "Lockfile not found." && exit 1; \
  fi

# Generate Prisma Client
RUN npx prisma generate


# Rebuild the source code only when needed
FROM base AS builder
WORKDIR /app
COPY --from=deps /app/node_modules ./node_modules
COPY . .

RUN npm run build


# Production image, copy all the files and run next
FROM base AS runner
WORKDIR /app

ENV NODE_ENV=production

RUN addgroup --system --gid 1001 nodejs
RUN adduser --system --uid 1001 nestjs

# Install openssl for Prisma in production environment
RUN apk add --no-cache openssl

COPY --from=builder --chown=nestjs:nodejs /app/dist ./dist
COPY --from=builder --chown=nestjs:nodejs /app/node_modules ./node_modules
COPY --from=builder --chown=nestjs:nodejs /app/package.json ./package.json
COPY --from=builder --chown=nestjs:nodejs /app/prisma ./prisma
# Copy migration scripts/assets if needed
# COPY --from=builder /app/migrations ./migrations

USER nestjs

EXPOSE 3001

CMD ["sh", "-c", "npx prisma db push && node dist/src/main"]
