package com.automatics.rdkb.tests.telemetry;

import java.util.List;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.constants.LinuxCommandConstants;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.telemetry.BroadBandTelemetryUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

public class BroadBandTelemetryBasicTests extends AutomaticsTestBase {
	
	
	/**
	 * Verify the CPU and Memory usage individually per process
	 * <ol>
	 * <li>verify the log file /rdklogs/logs/CPUInfo.txt.0 to find the CPU and
	 * Memory usage for each individual processes that are running</li>
	 * 
	 * @author Dipankur Nalui
	 * @refactor Athira
	 * 
	 *           </ol>
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
	@TestDetails(testUID = "TC-RDKB-CPU_AND_MEMORY-1001")
	public void testToVerifyCpuAndMemoryUsageIndividuallyPerProcess(Dut device) {

		// Variable Declaration begins
		String testCaseId = "TC-RDKB-CPU_AND_MEMORY-101";
		String stepNum = "s1";
		String errorMessage = null;
		String response = null;
		boolean status = false;
		// Variable Declaration Ends

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-CPU_AND_MEMORY-1001");
		LOGGER.info("TEST DESCRIPTION: Verify the CPU and Memory usage individually per process");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info(
				"1. verify the log file /rdklogs/logs/CPUInfo.txt.0 to find the CPU and Memory usage for each individual processes that are running");

		LOGGER.info("#######################################################################################");

		try {

			LOGGER.info("**********************************************************************************");

			stepNum = "s1";
			errorMessage = "Failed to verify CPU and Memory usage in the log file /rdklogs/logs/CPUInfo.txt.0 for only Atom based devices";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 1: DESCRIPTION : verify the log file /rdklogs/logs/CPUInfo.txt.0 to find the CPU and Memory usage for each individual processes that are running");
			LOGGER.info(
					"STEP 1: ACTION : Execute the following command: 1. ls -l /rdklogs/logs/CPUInfo.txt.0 2. ps | grep Ccsp 3. cat /rdklogs/logs/CPUInfo.txt.0");
			LOGGER.info(
					"STEP 1: EXPECTED : CPU and Memory usage should be displayed in the log file /rdklogs/logs/CPUInfo.txt.0 for only Atom based devices");
			LOGGER.info("**********************************************************************************");

			BroadBandResultObject result = null;
			if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
				errorMessage = "log file /rdklogs/logs/CPUInfo.txt.0 is not available";
				if (CommonUtils.isFileExists(device, tapEnv, BroadBandCommandConstants.FILE_PATH_TO_GET_CPU_INFO)) {
					response = tapEnv.executeCommandUsingSsh(device, LinuxCommandConstants.PS_COMMAND_FOR_CCSP_PROCESS);
					errorMessage = "Unable to get list of running process. Response - " + response;
					if (CommonMethods.isNotNull(response)) {
						result = verifyMemoryAndCpuUsageFromLogFile(device, tapEnv, response,
								BroadBandTestConstants.LIST_OF_PROCESS_FOR_CPU_MEM,
								BroadBandCommandConstants.FILE_PATH_TO_GET_CPU_INFO,
								BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
								BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
						if (result != null) {
							status = result.isStatus();
							errorMessage = result.getErrorMessage();
						}
					}
				}
			}

			if (status) {
				LOGGER.info(
						"STEP 1: ACTUAL : CPU and Memory usage for each individual process are displayed in the log file  /rdklogs/logs/CPUInfo.txt.0");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-CPU_AND_MEMORY-1001");
	}

	
	/**
	 * Verify the CPU and Memory usage Telemetry Data
	 * <ol>
	 * <li>Copy dcm.properties to /nvram folder and change the
	 * DCM_LOG_SERVER_URL</li>
	 * <li>Reboot the device and wait for IP acquisition</li>
	 * <li>Validate modified url in dcmscript.log file</li>
	 * <li>Verify whether telemetry is scheduled to 5 min in atom console</li>
	 * <li>Verify whether DCMresponse.txt file is available in the device under
	 * /nvram</li>
	 * <li>Verify whether cpu and memory telemetry data is available in the
	 * /rdklogs/logs/dcmscript.log</li>
	 * 
	 * @author Dipankur Nalui
	 * @refactor yamini.s
	 *           </ol>
	 */

	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
	@TestDetails(testUID = "TC-RDKB-CPU_AND_MEMORY-1002")
	public void testToVerifyCpuAndMemoryTelemetryData(Dut device) {

// Variable Declaration begins
		String testCaseId = "TC-RDKB-CPU_AND_MEMORY-102";
		String stepNum = "s1";
		String errorMessage = null;
		boolean status = false;
		String response = null;
// Boolean to store Telemetry 2 is enabled or not
		boolean isTelemetry2Enabled = false;
// Variable Declaration Ends

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-CPU_AND_MEMORY-1002");
		LOGGER.info("TEST DESCRIPTION: Verify the CPU and Memory usage individually per process");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info("STEP 1: Copy dcm.properties to /nvram folder and change the DCM_LOG_SERVER_URL");
		LOGGER.info("STEP 2: Reboot the device and wait for IP acquisition");
		LOGGER.info("STEP 3: Validate modified url in dcmscript.log file");
		LOGGER.info("STEP 4: Verify whether telemetry is scheduled to 5 min in atom console");
		LOGGER.info("STEP 5: Verify whether DCMresponse.txt file is available in the device under /nvram");
		LOGGER.info(
				"STEP 6: Verify whether cpu and memory telemetry data is available in the /rdklogs/logs/dcmscript.log");

		LOGGER.info("#######################################################################################");

		try {

			// Posting the profile along with the existing profile.
			LOGGER.info("PRE-CONDITION : DESCRIPTION : Upload telemetry profile to Xconf ");
			LOGGER.info("PRE-CONDITION : ACTION : Post request to Xconf");
			LOGGER.info("PRE-CONDITION : EXPECTED : Post request to Xconf should respond with HTTP 200 code ");

			errorMessage = "Unable to Post telemetry data to XConf";

			status = (BroadBandTelemetryUtils.postDataToProxyDcmServer(device, tapEnv, false,
					true) == BroadBandTestConstants.CONSTANT_200);

			if (status) {
				LOGGER.info("PRE-CONDITION : ACTUAL : Pre condition executed successfully");
			} else {
				LOGGER.error("PRE-CONDITION : ACTUAL : " + errorMessage);
				throw new TestException(errorMessage);
			}

			LOGGER.info("**********************************************************************************");

			stepNum = "s1";
			status = false;
			errorMessage = "Failed to Copy dcm.properties to /nvram folder and change the DCM_LOG_SERVER_URL";

			LOGGER.info("**********************************************");
			LOGGER.info(
					"STEP 1 : DESCRIPTION : Copy dcm.properties to /nvram folder and change the DCM_LOG_SERVER_URL");
			LOGGER.info("STEP 1 : ACTION : cp /etc/dcm.properties /nvram/");
			LOGGER.info("STEP 1 : EXPECTED: The file should be copied and updated");
			LOGGER.info("**********************************************");

			int validationStatus = BroadBandTelemetryUtils.copyAndUpdateDcmProperties(device, tapEnv);
			// updating the test case based on the return value from the helper
			// method
			if (BroadBandTestConstants.CONSTANT_0 == validationStatus) {
				errorMessage = "dcm.properties is not present, missing, in /etc/ folder";
			} else if (BroadBandTestConstants.CONSTANT_2 == validationStatus) {
				errorMessage = "Successfuly copied /etc/dcm.properties to /nvram folder, But Failed to update Xconf url!!!!";
			} else {
				status = true;
			}

			if (status) {
				LOGGER.info(
						"STEP 1: ACTUAL : Successfully copied dcm.properties file to /nvram and verified the DCM_LOG_SERVER_URL");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "s2";
			status = false;
			errorMessage = "Failed to reboot the device.";

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 2 : DESCRIPTION : Reboot the device and wait for IP acquisition");
			LOGGER.info("STEP 2 : ACTION : Execute the command: /sbin/reboot");
			LOGGER.info("STEP 2 : EXPECTED : Device should be rebooted");
			LOGGER.info("**********************************************************************************");

			status = BroadBandTelemetryUtils.rebootAndWaitForDeviceAccessible(tapEnv, device,
					BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS);

			if (status) {
				LOGGER.info("STEP 2: ACTUAL : Successfully reboot the STB and able to access the device ");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "s3";
			status = false;
			errorMessage = "Failed to verify the file with modified xconf url.";

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 3 : DESCRIPTION : Validate modified url in dcmscript.log file");
			LOGGER.info("STEP 3 : ACTION : Execute the command: cat /rdklogs/logs/dcmscript.log");
			LOGGER.info("STEP 3 : EXPECTED: File should have modified xconf url.");
			LOGGER.info("**********************************************************************************");

			try {
				BroadBandTelemetryUtils.verifyXconfDcmConfigurationUrlAndDownloadStatusFromDcmScriptLogs(device, tapEnv,
						BroadBandTestConstants.PROP_KEY_PROXY_XCONF_URL);
				status = true;
			} catch (TestException exception) {
				errorMessage = exception.getMessage();
			}

			if (status) {
				LOGGER.info("STEP 3: ACTUAL : Successfully verified that File contans modified xconf url.");
			} else {
				LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s4";
			status = false;
			errorMessage = "Failed to veirfy Cron job have waiting time of 5 min.";

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 4 : DESCRIPTION : Verify whether telemetry is scheduled to 5 min in atom console");
			LOGGER.info("STEP 4 : ACTION : Execute the command: crontab -l | grep -i dca");
			LOGGER.info("STEP 4 : EXPECTED: Cron job should have waiting time of 5 min.");
			LOGGER.info("**********************************************************************************");
			long startTime = System.currentTimeMillis();
			isTelemetry2Enabled = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_FOR_TELEMETRY_2_0_ENABLE, BroadBandTestConstants.TRUE,
					BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS, BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
			if (!isTelemetry2Enabled) {
				do {
					try {
						status = BroadBandTelemetryUtils.verifyDcmCronJobsConfiguredBasedOnDcmSettings(device, tapEnv,
								BroadBandTelemetryUtils.SCHEDULE_CRON_JOB_TIME_FOR_TELEMETRY);

					} catch (TestException exception) {
						errorMessage = exception.getMessage();
					}
				} while (!status
						&& (System.currentTimeMillis() - startTime) < BroadBandTestConstants.THREE_MINUTE_IN_MILLIS
						&& BroadBandCommonUtils.hasWaitForDuration(tapEnv,
								BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));

				if (status) {
					LOGGER.info("STEP 4: ACTUAL : Successfully verified that Cron job have waiting time of 5 min.");
				} else {
					LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			} else {
				LOGGER.info("STEP 4 IS NOT APPLICABLE IF TELEMETRY 2 IS ENABLED");
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			}
			stepNum = "s5";
			status = false;
			errorMessage = "Failed to verify the DCMresponse.txt file is available in the device under /nvram";

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 5 : DESCRIPTION : Verify whether DCMresponse.txt file is available in the device under /nvram");
			LOGGER.info("STEP 5 : ACTION : Execute the command: ls -l /nvram/DCMresponse.txt");
			LOGGER.info("STEP 5 : EXPECTED: The file should be available");
			LOGGER.info("**********************************************************************************");

			startTime = System.currentTimeMillis();
			do {
				LOGGER.info("Waiting for device to be accessed");
				status = CommonUtils.isFileExists(device, tapEnv, BroadBandTestConstants.FILE_NVRAM_DCMRESPONSE);
			} while (!status && (System.currentTimeMillis() - startTime) < BroadBandTestConstants.THREE_MINUTE_IN_MILLIS
					&& BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));

			if (status) {
				LOGGER.info(
						"STEP 5: ACTUAL : Successfully verified that the DCMresponse.txt file is available in the device under /nvram");
			} else {
				LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "s6";
			status = false;
			errorMessage = "Failed to verify the telemetry data in /rdklogs/logs/dcmscript.log";

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 6 : DESCRIPTION : Verify the telemetry data in /rdklogs/logs/dcmscript.log");
			LOGGER.info("STEP 6 : ACTION : Execute the command: cat /rdklogs/logs/dcmscript.log");
			LOGGER.info("STEP 6 : EXPECTED: telemetry data should be available in /rdklogs/logs/dcmscript.log");
			LOGGER.info("**********************************************************************************");
			BroadBandResultObject result = null;
			if (!isTelemetry2Enabled) {

				if (!CommonMethods.isAtomSyncAvailable(device, tapEnv)) {

					errorMessage = "log file /rdklogs/logs/dcmscript.log is not available";
					if (CommonUtils.isFileExists(device, tapEnv, BroadBandCommandConstants.LOG_FILE_DCM_SCRIPT)) {
						response = tapEnv.executeCommandUsingSsh(device,
								BroadBandTestConstants.PS_COMMAND_FOR_CCSP_PROCESS);
						errorMessage = "Unable to get list of running process. Response - " + response;
						if (CommonMethods.isNotNull(response)) {
							result = verifyMemoryAndCpuUsageFromLogFile(device, tapEnv, response,
									BroadBandTestConstants.LIST_OF_PROCESS_FOR_CPU_MEM,
									BroadBandCommandConstants.LOG_FILE_DCM_SCRIPT,
									BroadBandTestConstants.TWENTY_MINUTES_IN_MILLIS,
									BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
							if (result != null) {
								status = result.isStatus();
								errorMessage = result.getErrorMessage();
							}
						}
					}

				}
				if (status) {
					LOGGER.info(
							"STEP 6: ACTUAL : Successfully verified that the telemetry data is available in /rdklogs/logs/dcmscript.log");
				} else {
					LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			} else {
				LOGGER.info("STEP 6 IS NOT APPLICABLE IF TELEMETRY 2 IS ENABLED");
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			}
		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-CPU_AND_MEMORY-1002");
	}
	
	/*
	 * This method is to verify the telemetry marker for cpu and memory usage from
	 * the log file
	 * 
	 * @param device instance of {@link Dut}
	 * 
	 * @param tapApi instance of {@link AutomaticsTapApi}
	 * 
	 * @param response is the output of ps | grep ccsp command
	 * 
	 * @param listOfProcesses is the list of ccsp processes
	 * 
	 * @return status true if cpu and memory usage available in the log else false
	 * 
	 * @author Dipankur Nalui
	 * 
	 * @refactor Athira
	 * 
	 */

	public BroadBandResultObject verifyMemoryAndCpuUsageFromLogFile(Dut device, AutomaticsTapApi tapEnv,
			String response, List<String> listOfProcesses, String logFile, long pollDuration, long pollInterval) {

		BroadBandResultObject result = new BroadBandResultObject();
		boolean status = false;
		String logResponse = null;
		String memTelemetryData = null;
		String cpuTelemetryData = null;
		StringBuffer cpuUsage = new StringBuffer();
		StringBuffer memUsage = new StringBuffer();

		for (String process : listOfProcesses) {

			if (CommonUtils.isGivenStringAvailableInCommandOutput(response, process)) {
				LOGGER.info(process + " is running. Checking telemetry log for " + process);

				if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
					memTelemetryData = CommonMethods.concatStringUsingStringBuffer(process,
							CommonMethods.CHARACTER_UNDER_SCORE, BroadBandTestConstants.STRING_MEM);
					cpuTelemetryData = CommonMethods.concatStringUsingStringBuffer(process,
							CommonMethods.CHARACTER_UNDER_SCORE, BroadBandTestConstants.STRING_CPU);
				} else {
					memTelemetryData = CommonMethods.concatStringUsingStringBuffer(BroadBandTestConstants.STRING_MEM,
							CommonMethods.CHARACTER_UNDER_SCORE, process);
					cpuTelemetryData = CommonMethods.concatStringUsingStringBuffer(BroadBandTestConstants.STRING_CPU,
							CommonMethods.CHARACTER_UNDER_SCORE, process);
				}

				logResponse = BroadBandCommonUtils.searchLogFiles(tapEnv, device, memTelemetryData, logFile,
						pollDuration, pollInterval);
				status = CommonMethods.isNotNull(logResponse) && CommonMethods.patternMatcher(logResponse, process);
				if (!status) {
					memUsage.append(process);
					memUsage.append(BroadBandTestConstants.CHARACTER_COMMA);
				}

				logResponse = BroadBandCommonUtils.searchLogFiles(tapEnv, device, cpuTelemetryData, logFile,
						pollDuration, pollInterval);
				status = CommonMethods.isNotNull(logResponse) && CommonMethods.patternMatcher(logResponse, process);
				if (!status) {
					cpuUsage.append(process);
					memUsage.append(BroadBandTestConstants.CHARACTER_COMMA);
				}

			} else {
				LOGGER.info(process + " is not running. Skiping the telemetry log check for " + process);
			}
		}

		result.setStatus(CommonMethods.isNull(memUsage.toString()) && CommonMethods.isNull(cpuUsage.toString()));
		result.setErrorMessage("MEM Usage is not displayed for process - " + memUsage.toString()
				+ " CPU Usage is not displayed for process- " + cpuUsage.toString());

		return result;
	}


}
