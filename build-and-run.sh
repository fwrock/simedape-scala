#!/bin/bash
set -e

PROJECT_NAME="simedape"
SCALA_VERSION="3.3.5"

echo "🚀 Step 1: Cleaning old build..."
sbt clean

echo "📦 Step 2: Compiling and generating fat JAR..."
sbt assembly

JAR_FILE="target/scala-${SCALA_VERSION}/${PROJECT_NAME}.jar"

if [[ ! -f "$JAR_FILE" ]]; then
    echo "❌ Error: JAR not found at $JAR_FILE"
    exit 1
fi

echo "🐳 Step 3: Building Docker image..."
docker build -t ${PROJECT_NAME}:latest .

echo "📦 Step 4: Starting with docker compose..."
echo "   Make sure .env is configured (cp .env.example .env)"
echo "   The simulator's Docker network must exist before running."
docker compose up

echo "✅ SimEDaPE is running!"
echo "   Health:  http://localhost:${SIMEDAPE_HTTP_PORT:-9000}/health"
echo "   API:     http://localhost:${SIMEDAPE_HTTP_PORT:-9000}/api/v1/pipeline/status"
echo "   Logs:    docker compose logs -f simedape"
