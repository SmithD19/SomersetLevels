#!/bin/sh

echo "Launching model scripts on cluster..."
sbatch PresenceAbsence.sh
sbatch Abundance.sh
sbatch AbundanceHurdle.sh
echo "Done..."
