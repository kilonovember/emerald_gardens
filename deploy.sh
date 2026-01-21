#!/bin/bash

# 1. Navigate to the project directory
cd ~/datascience/emerald_gardens || { echo "Directory not found"; exit 1; }

# 2. Check if a commit message was provided
if [ -z "$1" ]
then
    echo "Error: No commit message provided."
    echo "Usage: ./deploy.sh 'Your commit message here'"
    exit 1
fi

# 3. Git commands
echo "Adding files..."
git add .

echo "Committing with message: $1"
git commit -m "$1"

echo "Pushing to GitHub..."
git push origin master  # Change 'main' to 'master' if your branch uses the old naming

echo "Done!"
