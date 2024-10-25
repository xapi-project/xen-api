type VMTelemetryFrequency string

const (
	// Run telemetry task daily
	VMTelemetryFrequencyDaily VMTelemetryFrequency = "daily"
	// Run telemetry task weekly
	VMTelemetryFrequencyWeekly VMTelemetryFrequency = "weekly"
	// The value does not belong to this enumeration
	VMTelemetryFrequencyUnrecognized VMTelemetryFrequency = "unrecognized"
)
