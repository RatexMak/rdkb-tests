/*
 * Copyright 2021 Comcast Cable Communications Management, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.automatics.rdkb.tests.telemetry;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.apache.http.HttpStatus;
import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.AVConstants;
import com.automatics.constants.AutomaticsConstants;
import com.automatics.constants.DataProviderConstants;
import com.automatics.constants.LinuxCommandConstants;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.enums.TR69ParamDataType;
import com.automatics.exceptions.TestException;
import com.automatics.providers.tr69.Parameter;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTelemetryConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandPreConditionUtils;
import com.automatics.rdkb.utils.BroadBandRfcFeatureControlUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.dmcli.DmcliUtils;
import com.automatics.rdkb.utils.telemetry.BroadBandTelemetryUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
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

	/**
	 * Test to verify Use new telemetry endpoint
	 * <ol>
	 * <li>verify that the TelemetryEndpoint.URL is disabled by default</li>
	 * <li>verify that the TelemetryEndpoint.Enable is disabled by default</li>
	 * <li>Verify that the old STBRTL url is present in
	 * /rdklogs/logs/dcmscript.log</li>
	 * <li>Set value of parameter
	 * Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.TelemetryEndpoint.URL to new
	 * STBRTL url</li>
	 * <li>reboot device and verify that
	 * Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.TelemetryEndpoint.URL is
	 * enabled</li>
	 * <li>Verify that the old STBRTL url is still present and
	 * /rdklogs/logs/dcmscript.log and should not be updated to new url</li>
	 * <li>set value of parameter
	 * Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.TelemetryEndpoint.Enable to
	 * true</li>
	 * <li>reboot and Verify that the telemetry endpoint url should be changed to
	 * new STBRTL url in /opt/logs/dcmscript.log</li>
	 * <li>Verify that the telemetry endpoint url should be changed to new STBRTL
	 * url in /rdklogs/logs/dcmscript.log</li>
	 * <li>Endpoint for telemetry should include a single 30 second timeout and is
	 * logged in /rdklogs/logs/dcmscript.log</li>
	 * </ol>
	 * 
	 * @author Krithiga Mukundan
	 * @refactor Said Hisham
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
	@TestDetails(testUID = "TC-RDKB-TELEMETRYENDPOINT-2250")
	public void testVerifyTelemetryEndpointUrl(Dut device) {

		// Variable Declaration begins
		String testCaseId = "TC-RDKB-TELEMETRYENDPOINT-250";
		String stepNum = "s1";
		String errorMessage = "";
		boolean status = false;
		boolean isTelemetryEndPointEnabled = false;
		boolean isTelemetryEnabled = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
				BroadBandWebPaConstants.WEBPA_PARAM_FOR_TELEMETRY_2_0_ENABLE, BroadBandTestConstants.TRUE,
				BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS, BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
		int preConStepNumber = 0;
		int stepNumber = 0;
		// Variable Declaration Ends

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-TELEMETRYENDPOINT-2250");
		LOGGER.info("TEST DESCRIPTION: Test to verify Use new telemetry endpoint");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info("Precondition 1. Verify that the TelemetryEndpoint.Enable is enabled by default");
		LOGGER.info("Precondition 2. Perform Reboot on the device if Telemetry T2 is enabled");
		LOGGER.info("1. verify that the TelemetryEndpoint.URL is disabled by default");
		LOGGER.info("2. verify that the TelemetryEndpoint.Enable is disabled by default");
		LOGGER.info("3. Verify that the old STBRTL url is present in /rdklogs/logs/dcmscript.log");
		LOGGER.info(
				"4. Set value of parameter Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.TelemetryEndpoint.URL to new STBRTL url");
		LOGGER.info(
				"5. reboot device and verify that Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.TelemetryEndpoint.URL is enabled");
		LOGGER.info(
				"6. Verify that the old STBRTL url is still present and /rdklogs/logs/dcmscript.log and should not be updated to new url");
		LOGGER.info(
				"7. set value of parameter Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.TelemetryEndpoint.Enable to true");
		LOGGER.info(
				"8. reboot and  Verify that the telemetry endpoint url should be changed to new in /opt/logs/dcmscript.log");
		LOGGER.info(
				"9. Verify that the telemetry endpoint url should be changed to new STBRTL url in /rdklogs/logs/dcmscript.log");
		LOGGER.info(
				"10. Endpoint for telemetry should include a single 30 second timeout and is logged in /rdklogs/logs/dcmscript.log");

		LOGGER.info("#######################################################################################");

		LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
		LOGGER.info("PRE-CONDITION STEPS");
		LOGGER.info("#######################################################################################");
		LOGGER.info("PRE-CONDITION : DESCRIPTION : Verify that the TelemetryEndpoint.Enable is enabled by default");
		LOGGER.info(
				"PRE-CONDITION : ACTION : Executing command:tr181 Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.TelemetryEndpoint.Enable");
		LOGGER.info(
				"PRE-CONDITION : EXPECTED : should return true as this telementryEndpoint.Enable is enabled by default");
		LOGGER.info("#######################################################################################");
		isTelemetryEndPointEnabled = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
				BroadBandWebPaConstants.WEBPA_PARAM_WIFI_TELEMETRY_ENDPOINT_ENABLE, BroadBandTestConstants.FALSE,
				BroadBandTestConstants.ONE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);

		if (isTelemetryEndPointEnabled) {
			LOGGER.info("PRE-CONDITION " + preConStepNumber
					+ " : ACTUAL : TelemetryEndpoint.Enable is disabled by default");
		} else {
			errorMessage = "TelemetryEndpoint.Enable is enabled by default but telemetry endpoint url is not "
					+ BroadbandPropertyFileHandler.getNewStbRtlUrl();
			String webPaResponse = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFI_TELEMETRY_ENDPOINTURL);
			status = BroadbandPropertyFileHandler.getNewStbRtlUrl().contains(webPaResponse);
			if (status) {
				LOGGER.info("PRE-CONDITION " + preConStepNumber
						+ " : ACTUAL : TelemetryEndpoint.Enable is enabled by default and telemetry endpoint url is "
						+ BroadbandPropertyFileHandler.getNewStbRtlUrl());
			} else {
				LOGGER.error("PRE-CONDITION " + preConStepNumber + " : ACTUAL : " + errorMessage);
				throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION " + preConStepNumber
						+ " : FAILED : " + errorMessage);
			}
		}

		if (isTelemetryEnabled) {

			BroadBandPreConditionUtils.preConditionToRebootAndWaitForIpAccusition(device, tapEnv,
					BroadBandTestConstants.CONSTANT_2);
		}
		LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");
		try {
			if (isTelemetryEndPointEnabled) {
				errorMessage = "telementryEndpoint is not null";
				status = false;

				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP 1: DESCRIPTION : verify that the TelemetryEndpoint.URL is disabled by default");
				LOGGER.info(
						"STEP 1: ACTION : Executing command:dmcli eRT getv Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.TelemetryEndpoint.URL");
				LOGGER.info("STEP 1: EXPECTED : should return null as this telementryEndpoint are disabled default");
				LOGGER.info("**********************************************************************************");

				status = CommonMethods.isNull(BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_WIFI_TELEMETRY_ENDPOINTURL));

				if (status) {
					LOGGER.info("STEP 1: ACTUAL : Default value for TelemetryEndpoint URL is NULL as expected.");
				} else {
					LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

				stepNum = "s2";
				errorMessage = "telementryEndpoint is not false";
				status = false;

				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP 2: DESCRIPTION : verify that the TelemetryEndpoint.Enable is disabled by default");
				LOGGER.info(
						"STEP 2: ACTION : Executing command:dmcli eRT getv Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.TelemetryEndpoint.Enable");
				LOGGER.info("STEP 2: EXPECTED : should return false as this telementryEndpoint are disabled default");
				LOGGER.info("**********************************************************************************");

				status = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_WIFI_TELEMETRY_ENDPOINT_ENABLE,
						BroadBandTestConstants.FALSE);

				if (status) {
					LOGGER.info("STEP 2: ACTUAL : Default value for TelemetryEndpoint Enable is False as expected");
				} else {
					LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

				stepNum = "s3";
				errorMessage = "old STBRTL url is not present in dcmscript.log";
				status = false;

				LOGGER.info("**********************************************************************************");
				LOGGER.info(
						"STEP 3: DESCRIPTION : Verify that the old STBRTL url is present in /rdklogs/logs/dcmscript.log");
				LOGGER.info("STEP 3: ACTION : Execute command:grep -i <old STBRTL url>  /rdklogs/logs/dcmscript.log");
				LOGGER.info("STEP 3: EXPECTED : <old STBRTL url> should be present in dcmscript.log");
				LOGGER.info("**********************************************************************************");

				if (!isTelemetryEnabled) {
					status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
							BroadbandPropertyFileHandler.getOldStbRtlUrl(), BroadBandTestConstants.DCMSCRIPT_LOG_FILE,
							BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
							BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
				} else {
					status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
							BroadbandPropertyFileHandler.getOldStbRtlUrl(),
							BroadBandTestConstants.FILE_PATH_TELEMETRY_2_0, BroadBandTestConstants.TEN_MINUTE_IN_MILLIS,
							BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
				}

				if (status) {
					LOGGER.info("STEP 3: ACTUAL : Old STBRTL URL is present in dcmscript.log");
				} else {
					LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
				stepNum = "s4";
				errorMessage = "Failed to set TelemetryEndpoint.URL to <new STBRTL url>";
				status = false;

				LOGGER.info("**********************************************************************************");
				LOGGER.info(
						"STEP 4: DESCRIPTION : Set value of parameter Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.TelemetryEndpoint.URL to <new STBRTL url>");
				LOGGER.info(
						"STEP 4: ACTION : Executing command:dmcli eRT setv Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.TelemetryEndpoint.URL string <new STBRTL url>");
				LOGGER.info(
						"STEP 4: EXPECTED : Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.TelemetryEndpoint.URL parameter should be set to <new STBRTL url>");
				LOGGER.info("**********************************************************************************");

				status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_WIFI_TELEMETRY_ENDPOINTURL,
						BroadBandTestConstants.CONSTANT_0, BroadbandPropertyFileHandler.getNewStbRtlUrl());
				if (status) {
					LOGGER.info(
							"STEP 4: ACTUAL :Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.TelemetryEndpoint.URL parameter should be set to <new STBRTL url>");
				} else {
					LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

				stepNum = "s5";
				errorMessage = "Unable to reboot the device properly";
				status = false;

				LOGGER.info("**********************************************************************************");
				LOGGER.info(
						"STEP 5: DESCRIPTION : reboot device and verify that Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.TelemetryEndpoint.URL is enabled");
				LOGGER.info(
						"STEP 5: ACTION : Execute command:1. reboot2. wait for 4min3. dmcli eRT getv Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.TelemetryEndpoint.URL");
				LOGGER.info(
						"STEP 5: EXPECTED : Parameter TelemetryEndpoint.URL should be updated with url <new STBRTL url>");
				LOGGER.info("**********************************************************************************");

				LOGGER.info("Rebooting the device and waiting for it to come up");

				if (CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv)) {
					LOGGER.info("Device is up after Reboot");
					errorMessage = "Failed to update TelemetryEndpoint.URL to  <new STBRTL url>";
					String webPaResponse = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_WIFI_TELEMETRY_ENDPOINTURL);
					status = BroadbandPropertyFileHandler.getNewStbRtlUrl().contains(webPaResponse);

				}

				if (status) {
					LOGGER.info(
							"STEP 5: ACTUAL :  Parameter TelemetryEndpoint.URL is updated with url <new STBRTL url>");
				} else {
					LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

				stepNum = "s6";
				errorMessage = "<old STBRTL url> is not present in dcmscript.log";
				status = false;

				LOGGER.info("**********************************************************************************");
				LOGGER.info(
						"STEP 6: DESCRIPTION : Verify that the old url <old STBRTL url>  is still present and /rdklogs/logs/dcmscript.log and should not be updated to new url");
				LOGGER.info("STEP 6: ACTION : Execute command:grep -i <old STBRTL url>  /rdklogs/logs/dcmscript.log");
				LOGGER.info(
						"STEP 6: EXPECTED : Telemetry end point should use old url <old STBRTL url> as the new url is not enabled yet");
				LOGGER.info("**********************************************************************************");
				if (!isTelemetryEnabled) {
					status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
							BroadbandPropertyFileHandler.getOldStbRtlUrl(), BroadBandTestConstants.DCMSCRIPT_LOG_FILE,
							BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
							BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
				} else {
					status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
							BroadbandPropertyFileHandler.getOldStbRtlUrl(),
							BroadBandTestConstants.FILE_PATH_TELEMETRY_2_0,
							BroadBandTestConstants.FIFTEEN_MINUTES_IN_MILLIS,
							BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
				}

				if (status) {
					LOGGER.info(
							"STEP 6: ACTUAL : Telemetry end point is using old url <old STBRTL url> as the new url is not enabled yet");
				} else {
					LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

				stepNum = "s7";
				errorMessage = "Failed to set TelemetryEndpoint.Enable to true";
				status = false;

				LOGGER.info("**********************************************************************************");
				LOGGER.info(
						"STEP 7: DESCRIPTION : set value of parameter Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.TelemetryEndpoint.Enable to true");
				LOGGER.info(
						"STEP 7: ACTION : Executing command:dmcli eRT setv tr181 Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.TelemetryEndpoint.Enable bool true");
				LOGGER.info(
						"STEP 7: EXPECTED : Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.TelemetryEndpoint.Enable parameter should be set to \"true\"");
				LOGGER.info("**********************************************************************************");

				status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_WIFI_TELEMETRY_ENDPOINT_ENABLE,
						BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE);

				if (status) {
					LOGGER.info(
							"STEP 7: ACTUAL : Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.TelemetryEndpoint.Enable parameter is set to TRUE");
				} else {
					LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

				stepNum = "s8";
				errorMessage = "Unable to reboot the device properly";

				status = false;

				LOGGER.info("**********************************************************************************");
				LOGGER.info(
						"STEP 8: DESCRIPTION : reboot and  Verify that the telemetry endpoint url should be changed to new url <new STBRTL url>   in /opt/logs/dcmscript.log");
				LOGGER.info(
						"STEP 8: ACTION : Execute command:1. reboot2. wait for 4mins. dmcli eRT getv tr181 Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.TelemetryEndpoint.Enable");
				LOGGER.info(
						"STEP 8: EXPECTED : Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.TelemetryEndpoint.Enable should be enabled");
				LOGGER.info("**********************************************************************************");

				LOGGER.info("Rebooting the device and waiting for it to come up");

				if (CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv)) {
					LOGGER.info("Device is up after Reboot");
					errorMessage = "Failed to enable TelemetryEndpoint.Enable";
					status = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_WIFI_TELEMETRY_ENDPOINT_ENABLE,
							BroadBandTestConstants.TRUE);
				}

				if (status) {
					LOGGER.info(
							"STEP 8: ACTUAL : Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.TelemetryEndpoint.Enable is enabled");
				} else {
					LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			} else {
				stepNumber = 1;
				while (stepNumber <= 8) {
					stepNum = "s" + stepNumber;
					errorMessage = "STEP " + stepNumber
							+ ": ACTUAL : NOT APPLICABLE AS TELEMETRY ENDPOINT ENABLED BY DEFAULT";

					tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
							errorMessage, false);
					stepNumber++;
				}
			}
			stepNum = "s9";
			errorMessage = "<new STBRTL url> is not present in dcmscript.log";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 9: DESCRIPTION : Verify that the telemetry endpoint url should be changed to new url <new STBRTL url> in /rdklogs/logs/dcmscript.log");
			LOGGER.info("STEP 9: ACTION : grep -i <new STBRTL url> /rdklogs/logs/dcmscript.log");
			LOGGER.info("STEP 9: EXPECTED : <new STBRTL url> should be present in dcmscript.log");
			LOGGER.info("**********************************************************************************");
			if (!isTelemetryEnabled) {
				status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
						BroadbandPropertyFileHandler.getNewStbRtlUrl(), BroadBandTestConstants.DCMSCRIPT_LOG_FILE,
						BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
			} else {
				status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
						BroadbandPropertyFileHandler.getOldStbRtlUrl(), BroadBandTestConstants.FILE_PATH_TELEMETRY_2_0,
						BroadBandTestConstants.FIFTEEN_MINUTES_IN_MILLIS,
						BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
			}
			if (status) {
				LOGGER.info(
						"STEP 9: ACTUAL : <new STBRTL url> is be present in dcmscript.log/ <old STBRTL url> is present in telenmetry 2.0 ");
			} else {
				LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
			stepNum = "s10";
			errorMessage = "Failed to log 30sec timeout in  /rdklogs/logs/dcmscript.log";

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 10: DESCRIPTION : Endpoint for telemetry should include a single 30 second timeout and is logged in /rdklogs/logs/dcmscript.log");
			LOGGER.info("STEP 10: ACTION : grep -i \"connect-timeout 30 -m 30\"  /rdklogs/logs/dcmscript.log");
			LOGGER.info(
					"STEP 10: EXPECTED : Endpoint for telemetry has a single 30 second timeout and is logged in  /rdklogs/logs/dcmscript.log");
			LOGGER.info("**********************************************************************************");
			if (!isTelemetryEnabled) {
				status = false;

				status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
						BroadBandTelemetryConstants.ENDPOINT_TIMEOUT, BroadBandTestConstants.DCMSCRIPT_LOG_FILE,
						BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
			}
			if (status) {
				LOGGER.info(
						"STEP 10: ACTUAL : Endpoint for telemetry has a single 30 second timeout and is logged in  /rdklogs/logs/dcmscript.log");
			} else {
				LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

		} catch (Exception e) {
			errorMessage = e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		} finally {
			if (isTelemetryEndPointEnabled) {
				LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
				LOGGER.info("POST-CONDITION STEPS");
				LOGGER.info(
						"POST-CONDITION : DESCRIPTION : Revert back Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.TelemetryEndpoint.URL and Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.TelemetryEndpoint.Enable to default values  ");
				LOGGER.info(
						"POST-CONDITION : ACTION : Set Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.TelemetryEndpoint.URL to null and Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.TelemetryEndpoint.Enable to false and reboot the device ");
				LOGGER.info(
						"POST-CONDITION : EXPECTED : Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.TelemetryEndpoint.URL should be set to null and Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.TelemetryEndpoint.Enable should be set to false");
				if (BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_WIFI_TELEMETRY_ENDPOINT_ENABLE,
						BroadBandTestConstants.TRUE)) {
					status = DmcliUtils.setParameterValueUsingDmcliCommand(device, tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_WIFI_TELEMETRY_ENDPOINTURL,
							BroadBandTestConstants.DMCLI_SUFFIX_TO_SET_STRING_PARAMETER,
							BroadBandTestConstants.DMCLI_NULL_VALUE)
							&& BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
									BroadBandWebPaConstants.WEBPA_PARAM_WIFI_TELEMETRY_ENDPOINT_ENABLE,
									BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.FALSE)
							&& CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv);
				} else {
					status = DmcliUtils.setParameterValueUsingDmcliCommand(device, tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_WIFI_TELEMETRY_ENDPOINTURL,
							BroadBandTestConstants.DMCLI_SUFFIX_TO_SET_STRING_PARAMETER,
							BroadBandTestConstants.DMCLI_NULL_VALUE);

				}

				if (status) {
					LOGGER.info(
							"POST-CONDITION : ACTUAL : Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.TelemetryEndpoint.URL and Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.TelemetryEndpoint.Enable set to their default values");
				} else {
					LOGGER.error(
							"POST-CONDITION : ACTUAL : Unable to change Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.TelemetryEndpoint.URL and Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.TelemetryEndpoint.Enable to default ");
				}
				LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
				LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
			}
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-TELEMETRYENDPOINT-2250");
	}

	/**
	 * Verify TR-181 message-bus as new data source for telemetry
	 * <ol>
	 * <li>Verify Telemetry message bus source feature is enabled by default</li>
	 * <li>Wait for http response code 200 in dcmscript.log</li>
	 * <li>Wait for \"BandSteeringEnabledTest\" marker to appear</li>
	 * <li>Verify value of parameter uploaded in telemetry is same as actual
	 * parameter value</li>
	 * <li>Verify log message for telemetry data source not found for invalid
	 * parameter</li>
	 * <li>Verify invalid parameter telemetry marker does not appear in
	 * dcmscript.log</li>
	 * <li>Wait for 'TEST_MULTI_INSTANCE' marker to appear</li>
	 * <li>Verify log message for skipping telemetry object for invalid format
	 * parameter</li>
	 * <li>Verify invalid Multi instance parameter telemetry marker does not appear
	 * in dcmscript.log</li>
	 * <li>Disable Telemetry message bus source parameter</li>
	 * <li>Check disabled log message in dcmProcessing.log</li>
	 * <li>Enable Telemetry message bus source parameter</li>
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * 
	 * @author Ashwin sankara
	 * @refactor yamini.s
	 */
	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
	@TestDetails(testUID = "TC-RDKB-TELEMETRY-1004")
	public void testVerifyTR181MessageBusTelemetry(Dut device) {

		// Variable Declaration begins
		String testCaseId = "TC-RDKB-TELEMETRY-104";
		String stepNum = "s1";
		String errorMessage = null;
		String response = null;
		String paramValue = null;
		boolean status = false;
		long startTime = 0L;
		List<String> verifySplunkLog = new ArrayList<>();
		boolean isTelemetryEnabled = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
				BroadBandWebPaConstants.WEBPA_PARAM_FOR_TELEMETRY_2_0_ENABLE, BroadBandTestConstants.TRUE,
				BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS, BroadBandTestConstants.TEN_SECOND_IN_MILLIS);

		// Variable Declation Ends

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-TELEMETRY-1004");
		LOGGER.info("TEST DESCRIPTION: Verify TR-181 message-bus as new data source for telemetry");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info(
				"Precondition1: Configure telemetry payload data to upload BandSteeringEnable parameter and invalid parameter values");
		LOGGER.info("STEP 1. Verify Telemetry message bus source feature is enabled by default");
		LOGGER.info("STEP 2. Wait for http response code 200 in dcmscript.log");
		LOGGER.info("STEP 3. Wait for \"BandSteeringEnabledTest\" marker to appear");
		LOGGER.info("STEP 4. Verify value of parameter uploaded in telemetry is same as actual parameter value");
		LOGGER.info("STEP 5. Verify log message for telemetry data source not found for invalid parameter");
		LOGGER.info("STEP 6. Verify invalid parameter telemetry marker does not appear in dcmscript.log");
		LOGGER.info("STEP 7. Wait for 'TEST_MULTI_INSTANCE' marker to appear");
		LOGGER.info("STEP 8. Verify log message for skipping telemetry object for invalid format parameter");
		LOGGER.info(
				"STEP 9. Verify invalid Multi instance parameter telemetry marker does not appear in dcmscript.log");
		LOGGER.info("STEP 10. Disable Telemetry message bus source parameter");
		LOGGER.info("STEP 11. Check disabled log message in dcmProcessing.log");
		LOGGER.info("STEP 12. Enable Telemetry message bus source parameter");

		LOGGER.info("#######################################################################################");

		try {

			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			LOGGER.info(
					"PRE-CONDITION : DESCRIPTION : Configure telemetry payload data to upload BandSteeringEnable parameter and invalid parameter values");
			LOGGER.info("PRE-CONDITION : ACTION : Post payload data, copy and update dcm.properties and reboot");
			LOGGER.info("PRE-CONDITION : EXPECTED : Device rebooted successfully after configuring telemetry data");

			try {
				BroadBandTelemetryUtils.configureTelemetryProfiles(device, tapEnv, false);
				BroadBandCommonUtils.rebootDeviceAsPreCondition(tapEnv, device);
				status = true;
			} catch (TestException e) {
				errorMessage = BroadBandTestConstants.PRE_CONDITION_ERROR + e.getMessage();
			}

			if (status) {
				LOGGER.info("PRE-CONDITION : ACTUAL : Pre condition executed successfully");
			} else {
				LOGGER.error("PRE-CONDITION : ACTUAL : " + errorMessage);
				throw new TestException(errorMessage);
			}

			LOGGER.info("**********************************************************************************");

			errorMessage = "Default value of Telemetry message bus source parameter is not true";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION : Verify Telemetry message bus source feature is enabled by default");
			LOGGER.info(
					"STEP 1: ACTION : Execute command to get value of Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.Telemetry.MessageBusSource.Enable");
			LOGGER.info("STEP 1: EXPECTED : Default value of parameter is true");
			LOGGER.info("**********************************************************************************");

			response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_TELEMETRY_MESSAGE_BUS_SOURCE);
			status = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(BroadBandTestConstants.TRUE);

			if (status) {
				LOGGER.info("STEP 1: ACTUAL : Default value of parameter is true");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			stepNum = "s2";
			errorMessage = null;
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION : Wait for http response code 200 in dcmscript.log");
			LOGGER.info("STEP 2: ACTION : Execute command: cat /rdklogs/logs/dcmscript.log");
			LOGGER.info("STEP 2: EXPECTED : Log message with http code 200 exists");
			LOGGER.info("**********************************************************************************");
			startTime = System.currentTimeMillis();
			if (!isTelemetryEnabled) {
				try {
					BroadBandTelemetryUtils.verifyXconfDcmConfigurationUrlAndDownloadStatusFromDcmScriptLogs(device,
							tapEnv, BroadBandTestConstants.PROP_KEY_PROXY_XCONF_URL);
					status = true;
				} catch (TestException e) {
					errorMessage = e.getMessage();
				}
			} else {
				do {
					response = tapEnv.executeCommandUsingSsh(device,
							BroadBandTestConstants.CMD_GET_REPORT_SENT_SUCCESS_MESSAGE_FROM_LOGS);
					status = CommonMethods.isNotNull(response) && CommonMethods.patternMatcher(response,
							BroadBandTestConstants.STRING_TELEMETRY_REPORT_SUCCESS);
				} while (!status
						&& (System.currentTimeMillis() - startTime) < BroadBandTestConstants.TWENTY_MINUTES_IN_MILLIS
						&& BroadBandCommonUtils.hasWaitForDuration(tapEnv,
								BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS));
			}
			if (status) {
				LOGGER.info("STEP 2: ACTUAL : Log message with http code 200 exists");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "s3";
			errorMessage = "Unable to find telemetry marker 'BandSteeringEnabledTest' in dcmscript.log";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 3: DESCRIPTION : Wait for 'BandSteeringEnabledTest' marker to appear");
			LOGGER.info(
					"STEP 3: ACTION : Execute command: grep -i 'BandSteeringEnabledTest' /rdklogs/logs/dcmscript.log");
			LOGGER.info("STEP 3: EXPECTED : Telemetry marker 'BandSteeringEnabledTest' exists in dcmscript.log");
			LOGGER.info("**********************************************************************************");
			if (!isTelemetryEnabled) {
				response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
						BroadBandTestConstants.BAND_STEERING_ENABLED_TEST,
						BroadBandCommandConstants.LOG_FILE_DCM_SCRIPT, BroadBandTestConstants.TEN_MINUTE_IN_MILLIS,
						BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
				if (CommonMethods.isNotNull(response)) {
					errorMessage = "Unable to find value of parameter present in telemetry marker for BandSteeringEnable parameter";
					response = CommonMethods.patternFinder(response,
							BroadBandTestConstants.PATTERN_MATCHER_BANDSTEERING_TESTHEADER_VALUE);
					status = CommonMethods.isNotNull(response);
				}
			} else {
				if (!DeviceModeHandler.isBusinessClassDevice(device)) {
					response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_RDKCENTRAL_MESH_ENABLE);
					if (CommonMethods.isNotNull(response) && response.equalsIgnoreCase(BroadBandTestConstants.TRUE)) {
						status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
								BroadBandWebPaConstants.WEBPA_PARAM_BAND_STEERING_ENABLE, BroadBandTestConstants.FALSE,
								BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
								BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
					} else {
						status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
								BroadBandWebPaConstants.WEBPA_PARAM_BAND_STEERING_ENABLE, BroadBandTestConstants.TRUE,
								BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
								BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
					}
				} else {
					status = CommonMethods.isNotNull(BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device,
							tapEnv, BroadBandWebPaConstants.WEBPA_PARAM_BAND_STEERING_ENABLE));
				}
				response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_BAND_STEERING_ENABLE);
			}
			if (status) {
				LOGGER.info("STEP 3: ACTUAL : Telemetry marker 'BandSteeringEnabledTest' exists in dcmscript.log");
			} else {
				LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			stepNum = "s4";
			errorMessage = "Obtained null value of BandSteeringEnable parameter";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 4: DESCRIPTION : Verify value of parameter uploaded in telemetry is same as actual parameter value");
			LOGGER.info(
					"STEP 4: ACTION : Execute command to get value of Device.WiFi.X_RDKCENTRAL-COM_BandSteering.Enable and compare with telemetry marker value");
			LOGGER.info("STEP 4: EXPECTED : Value of parameter is same as value uploaded in telemetry");
			LOGGER.info("**********************************************************************************");

			paramValue = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_BAND_STEERING_ENABLE);
			if (CommonMethods.isNotNull(paramValue)) {
				errorMessage = "Value of parameter - " + paramValue + " is not the same as value in telemetry marker - "
						+ response;
				status = paramValue.equalsIgnoreCase(response);
			}

			if (status) {
				LOGGER.info("STEP 4: ACTUAL : Value of parameter is same as value uploaded in telemetry");
			} else {
				LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			if (isTelemetryEnabled) {
				LOGGER.info("Steps 5 to 9 are not applicable when T2 is enabled");
				LOGGER.info("#######################################################################################");
				tapEnv.updateExecutionForAllStatus(device, testCaseId, "s5", ExecutionStatus.NOT_APPLICABLE,
						"N/A for Business class devices", false);
				tapEnv.updateExecutionForAllStatus(device, testCaseId, "s6", ExecutionStatus.NOT_APPLICABLE,
						"N/A for Business class devices", false);
				tapEnv.updateExecutionForAllStatus(device, testCaseId, "s7", ExecutionStatus.NOT_APPLICABLE,
						"N/A for Business class devices", false);
				tapEnv.updateExecutionForAllStatus(device, testCaseId, "s8", ExecutionStatus.NOT_APPLICABLE,
						"N/A for Business class devices", false);
				tapEnv.updateExecutionForAllStatus(device, testCaseId, "s9", ExecutionStatus.NOT_APPLICABLE,
						"N/A for Business class devices", false);
			} else {

				stepNum = "s5";
				errorMessage = "Unable to find data source not found log message for invalid parameter - Device.WiFi.AccessPoint.1.AssociatedDevice.1.MACAddress in dcmProcessing.log";
				status = false;

				LOGGER.info("**********************************************************************************");
				LOGGER.info(
						"STEP 5: DESCRIPTION : Verify log message for telemetry data source not found for invalid parameter");
				LOGGER.info(
						"STEP 5: ACTION : Execute command: grep \"Telemetry data source not found. Type = <message_bus>. Content string = Device.WiFi.AccessPoint.1.AssociatedDevice.1.MACAddress\" /rdklogs/logs/dcmProcessing.log");
				LOGGER.info("STEP 5: EXPECTED : Log message for data source not found exists in dcmProcessing.log");
				LOGGER.info("**********************************************************************************");

				status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFilesInAtomOrArmConsoleByPolling(device,
						tapEnv, BroadBandTraceConstants.LOG_MESSAGE_TELEMETRY_DATA_SOURCE_NOT_FOUND_INVALID_PARAM,
						BroadBandCommandConstants.LOG_FILE_DCMPROCESSING, BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS,
						BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));

				if (status) {
					LOGGER.info("STEP 5: ACTUAL : Log message for data source not found exists in dcmProcessing.log");
				} else {
					LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

				stepNum = "s6";
				errorMessage = "Telemetry marker for invalid parameter - 'Device.WiFi.AccessPoint.1.AssociatedDevice.1.MACAddress' is present in dcmscript.log";
				status = false;

				LOGGER.info("**********************************************************************************");
				LOGGER.info(
						"STEP 6: DESCRIPTION : Verify invalid parameter telemetry marker does not appear in dcmscript.log");
				LOGGER.info(
						"STEP 6: ACTION : Execute command: grep -i \"TestInvalidParam_split\" /rdklogs/logs/dcmscript.log");
				LOGGER.info("STEP 6: EXPECTED : Invalid telemetry marker is not present in dcmscript.log");
				LOGGER.info("**********************************************************************************");

				status = CommonMethods.isNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
						BroadBandTestConstants.TEST_INVALID_PARAM, BroadBandTestConstants.DCMSCRIPT_LOG_FILE));

				if (status) {
					LOGGER.info("STEP 6: ACTUAL : Invalid telemetry marker is not present in dcmscript.log");
				} else {
					LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

				LOGGER.info("**********************************************************************************");

				stepNum = "s7";
				errorMessage = "Unable to find telemetry marker 'TEST_MULTI_INSTANCE' in dcmscript.log";
				status = false;

				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP 7: DESCRIPTION : Wait for 'TEST_MULTI_INSTANCE' marker to appear");
				LOGGER.info(
						"STEP 7: ACTION : Execute command: grep -i 'TEST_MULTI_INSTANCE' /rdklogs/logs/dcmscript.log");
				LOGGER.info("STEP 7: EXPECTED : Telemetry marker 'TEST_MULTI_INSTANCE' exists in dcmscript.log");
				LOGGER.info("**********************************************************************************");

				status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
						BroadBandTestConstants.TEST_MULTI_INSTANCE, BroadBandCommandConstants.LOG_FILE_DCM_SCRIPT,
						BroadBandTestConstants.ONE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));

				if (status) {
					LOGGER.info("STEP 7: ACTUAL : Telemetry marker 'TEST_MULTI_INSTANCE' exists in dcmscript.log");
				} else {
					LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

				stepNum = "s8";
				errorMessage = "Unable to find skipping telemetry object log message for invalid format parameter - Device.WiFi.AccessPoint.{i}.AssociatedDevice.{i}.MACAddress in dcmProcessing.log";
				status = false;

				LOGGER.info("**********************************************************************************");
				LOGGER.info(
						"STEP 8: DESCRIPTION : Verify log message for skipping telemetry object for invalid format parameter");
				LOGGER.info(
						"STEP 8: ACTION : Execute command: grep \"Skipping Telemetry object due to invalid format. Type = <message_bus>. Content string = Device.WiFi.AccessPoint.{i}.AssociatedDevice.{i}.MACAddress\" /rdklogs/logs/dcmProcessing.log");
				LOGGER.info("STEP 8: EXPECTED : Log message for skipping telemetry object exists in dcmProcessing.log");
				LOGGER.info("**********************************************************************************");

				status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFilesInAtomOrArmConsoleByPolling(device,
						tapEnv, BroadBandTraceConstants.LOG_MESSAGE_SKIPPING_TELEMETRY_OBJECT_INVALID_FORMAT,
						BroadBandCommandConstants.LOG_FILE_DCMPROCESSING, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
						BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));

				if (status) {
					LOGGER.info(
							"STEP 8: ACTUAL : Log message for skipping telemetry object exists in dcmProcessing.log");
				} else {
					LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

				stepNum = "s9";
				errorMessage = "Telemetry marker for invalid Multi instance parameter - 'Device.WiFi.AccessPoint.{i}.AssociatedDevice.{i}.MACAddress' is present in dcmscript.log";
				status = false;

				LOGGER.info("**********************************************************************************");
				LOGGER.info(
						"STEP 9: DESCRIPTION : Verify invalid Multi instance parameter telemetry marker does not appear in dcmscript.log");
				LOGGER.info(
						"STEP 9: ACTION : Execute command: grep -i \"TestInvalidParam_Multi\" /rdklogs/logs/dcmscript.log");
				LOGGER.info(
						"STEP 9: EXPECTED : Invalid Multi instance parameter telemetry marker is not present in dcmscript.log");
				LOGGER.info("**********************************************************************************");

				status = CommonMethods.isNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
						BroadBandTestConstants.TEST_INVALID_PARAM_MULTI, BroadBandTestConstants.DCMSCRIPT_LOG_FILE));

				if (status) {
					LOGGER.info(
							"STEP 9: ACTUAL : Invalid Multi instance parameter telemetry marker is not present in dcmscript.log");
				} else {
					LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			}
			stepNum = "s10";
			errorMessage = "Unable to set value of Telemetry message bus source parameter to false";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 10: DESCRIPTION : Disable Telemetry message bus source parameter");
			LOGGER.info(
					"STEP 10: ACTION : Execute command to set false value for Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.Telemetry.MessageBusSource.Enable");
			LOGGER.info("STEP 10: EXPECTED : Value of parameter set to false successfully");
			LOGGER.info("**********************************************************************************");

			status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_TELEMETRY_MESSAGE_BUS_SOURCE, BroadBandTestConstants.CONSTANT_3,
					BroadBandTestConstants.FALSE);

			if (status) {
				LOGGER.info("STEP 10: ACTUAL : Value of parameter set to false successfully");
			} else {
				LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			stepNum = "s11";
			errorMessage = "Unable to find telemetry message bus source disabled log message in dcmProcessing.log";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 11: DESCRIPTION : Check disabled log message in dcmProcessing.log");
			LOGGER.info(
					"STEP 11: ACTION : Execute command: grep \"TR181 MessageBusSource is disabled via RFC\" /rdklogs/logs/dcmProcessing.log");
			LOGGER.info("STEP 11: EXPECTED : Log message is present in dcmProcessing.log");
			LOGGER.info("**********************************************************************************");
			if (!isTelemetryEnabled) {
				status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFilesInAtomOrArmConsoleByPolling(device,
						tapEnv, BroadBandTraceConstants.LOG_MESSAGE_TELEMETRY_MESSAGE_BUS_SOURCE_DISABLED,
						BroadBandCommandConstants.LOG_FILE_DCMPROCESSING, BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS,
						BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));

				if (status) {
					LOGGER.info("STEP 11: ACTUAL : Log message is present in dcmProcessing.log");
				} else {
					LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			} else {

				errorMessage = "Custom profile steps not applicable for telemtry 2.0 ";
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);

			}

			stepNum = "s12";
			errorMessage = "Unable to set value of Telemetry message bus source parameter to true";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 12: DESCRIPTION : Enable Telemetry message bus source parameter");
			LOGGER.info(
					"STEP 12: ACTION : Execute command to set true value for Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.Telemetry.MessageBusSource.Enable");
			LOGGER.info("STEP 12: EXPECTED : Value of parameter set to true successfully");
			LOGGER.info("**********************************************************************************");

			status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_TELEMETRY_MESSAGE_BUS_SOURCE, BroadBandTestConstants.CONSTANT_3,
					BroadBandTestConstants.TRUE);

			if (status) {
				LOGGER.info("STEP 12: ACTUAL : Value of parameter set to true successfully");
			} else {
				LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, false, errorMessage,
					false);
		} finally {

			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS");
			LOGGER.info("POST-CONDITION : DESCRIPTION : Remove nvram telemetry settings and reboot");
			LOGGER.info(
					"POST-CONDITION : ACTION : Remove DCMSettings.conf files in /nvram or /tmp and dcm.properties in /nvram and reboot");
			LOGGER.info("POST-CONDITION : EXPECTED : Post conditions executed successfully");

			status = BroadBandTelemetryUtils.clearTelemetryConfiguration(tapEnv, device)
					&& BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_TELEMETRY_MESSAGE_BUS_SOURCE,
							BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE,
							BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS)
					&& CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv);

			if (status) {
				LOGGER.info("POST-CONDITION : ACTUAL : Post condition executed successfully");
			} else {
				LOGGER.error("POST-CONDITION : ACTUAL : Post condition failed");
			}
			LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-TELEMETRY-1004");
	}

	/**
	 * Test Case : Verify telemetry/logging for memory fragmentation details
	 *
	 * <p>
	 * STEPS:
	 * </p>
	 * <ol>
	 * <li>Set and verify CPU Memory fragmentation interval as 121 using webpa</li>
	 * <li>Set CPU Memory fragmentation interval as 1 using RFC</li>
	 * <li>Verify CPU memory fragmentation configuration updated through RFC in
	 * dcmrfc.log file</li>
	 * <li>Verify CPU Memory fragmentation interval changed to 1 hour interval</li>
	 * <li>Verify cron job is scheduled</li>
	 * <li>Verify CPU fragmentation details log message in CPUInfo.txt.0 file</li>
	 * <li>Verify Log message format in CPUInfo.txt.0 file</li>
	 * <li>Get CPU fragmentation details from /proc/buddyinfo</li>
	 * <li>Verify default primary and secondary CPU fragmentation zone values using
	 * webpa</li>
	 * <li>POST-CONDITION 1 : Set CPU fragmentation interval to default</li>
	 * <li>POST-CONDITION 2 : Begin Broadband Device Reactivation/li>
	 * <li>POST-CONDITION 3 : Revert the status of Periodic firmware check to
	 * default value</li>
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * 
	 * @author Betel Costrow
	 * @refactor Said Hisham
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
	@TestDetails(testUID = "TC-RDKB-TELEMETRY-1006")
	public void testToVerifyTelemetryMemoryFragmentationDetails(Dut device) {
		// Variable Declaration begins
		String testCaseId = "TC-RDKB-TELEMETRY-106";
		String stepNum = "s1";
		String errorMessage = null;
		boolean status = false;
		String response = null;
		int postConditionStepNum = BroadBandTestConstants.CONSTANT_1;
		String marginValue = BroadBandTestConstants.STRING_VALUE_12 + BroadBandTestConstants.STRING_VALUE_ONE;
		String pattenMatcher = null;
		String searchLogMessage = null;
		List<String> arrPrimaryZones = new ArrayList<String>();
		List<String> arrSecondaryZones = new ArrayList<String>();
		List<String> arrPrimaryValues = new ArrayList<String>();
		List<String> arrSecondaryValues = new ArrayList<String>();
		List<String> arrBuddyInfo = new ArrayList<String>();
		List<String> arrBuddyInfoAtomDevice = new ArrayList<String>();
		String[] arrExecuteMultiplePrimaryParams = new String[4];
		arrExecuteMultiplePrimaryParams[0] = BroadBandWebPaConstants.WEBPA_PARAM_PRIMARY_CPU_MEMFRAG_DMA;
		arrExecuteMultiplePrimaryParams[1] = BroadBandWebPaConstants.WEBPA_PARAM_PRIMARY_CPU_MEMFRAG_DMA32;
		arrExecuteMultiplePrimaryParams[2] = BroadBandWebPaConstants.WEBPA_PARAM_PRIMARY_CPU_MEMFRAG_NORMAL;
		arrExecuteMultiplePrimaryParams[3] = BroadBandWebPaConstants.WEBPA_PARAM_PRIMARY_CPU_MEMFRAG_HIGHMEM;
		String arrExecuteMultipleSecondaryParams[] = new String[4];
		arrExecuteMultipleSecondaryParams[0] = BroadBandWebPaConstants.WEBPA_PARAM_SECONDARY_CPU_MEMFRAG_DMA;
		arrExecuteMultipleSecondaryParams[1] = BroadBandWebPaConstants.WEBPA_PARAM_SECONDARY_CPU_MEMFRAG_DMA32;
		arrExecuteMultipleSecondaryParams[2] = BroadBandWebPaConstants.WEBPA_PARAM_SECONDARY_CPU_MEMFRAG_NORMAL;
		arrExecuteMultipleSecondaryParams[3] = BroadBandWebPaConstants.WEBPA_PARAM_SECONDARY_CPU_MEMFRAG_HIGHMEM;
		boolean defaultValue = false;
		boolean removingRfcProfile = false;
		boolean postExecution = false;
		boolean isFactoryReset = false;
		String fileName = null;
		// Variable Declaration Ends
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-TELEMETRY-1006");
		LOGGER.info("TEST DESCRIPTION: Verify telemetry/logging for memory fragmentation details");
		LOGGER.info("TEST STEPS : ");

		LOGGER.info("1. Set and verify CPU Memory fragmentation interval as 121 using webpa  ");
		LOGGER.info("2. Set CPU Memory fragmentation interval as 1 using RFC ");
		LOGGER.info("3. Verify CPU memory fragmentation configuration updated through RFC in dcmrfc.log file");
		LOGGER.info("4. Verify CPU Memory fragmentation interval changed to 1 hour interval");
		LOGGER.info("5. Verify  cron job is scheduled");
		LOGGER.info("6. Verify CPU fragmentation details log message in CPUInfo.txt.0 file");
		LOGGER.info("7. Verify Log message format in CPUInfo.txt.0 file");
		LOGGER.info("8. Get CPU fragmentation details from /proc/buddyinfo");
		LOGGER.info("9. Verify default primary and secondary CPU fragmentation zone values using webpa");
		LOGGER.info("POST-CONDITION 1 : Set CPU fragmentation interval to default");
		LOGGER.info("POST-CONDITION 2 : Begin Broadband Device Reactivation");
		LOGGER.info("POST-CONDITION 3 : Revert the status of Periodic firmware check to default value");
		LOGGER.info("#######################################################################################");
		try {

			/**
			 * Step 1 : Set and verify CPU Memory fragmentation interval as 121 using webpa
			 */
			stepNum = "s1";
			errorMessage = "Unable to Change CPU Memory fragmentation interval as 121 using webpa ";
			status = false;
			LOGGER.info("***************************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION : Set and verify CPU Memory fragmentation interval as 121 using webpa  ");
			LOGGER.info(
					"STEP 1: ACTION : Execute webpa set command:parameter: Device.SelfHeal.X_RDKCENTRAL-COM_CpuMemFragIntervaldata type: unsignedintvalue: 121");
			LOGGER.info("STEP 1: EXPECTED : Webpa set operation should fail.");
			LOGGER.info("***************************************************************************************");
			status = !BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_CPU_FRAGMENTATION_INTERVAL, BroadBandTestConstants.CONSTANT_2,
					marginValue);
			if (status) {
				LOGGER.info(
						"STEP 1: ACTUAL : Not able to set CPU memory fragmentation interval as 121 because 1 to 120 can set as CPU memory fragmentation interval");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}
			LOGGER.info("***************************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			/**
			 * Step 2 : Set CPU Memory fragmentation interval as 1 using webpa
			 */
			stepNum = "s2";
			errorMessage = "Unable to set CPU Memory fragmentation interval as 1 using RFC  ";
			status = false;
			LOGGER.info("***************************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION : Set CPU Memory fragmentation interval as 1 using RFC ");
			LOGGER.info("STEP 2: ACTION : Update CPU memory fragmentation interval value using RFC");
			LOGGER.info("STEP 2: EXPECTED :Parameter should updated with new value");
			LOGGER.info("***************************************************************************************");

			status = BroadBandRfcFeatureControlUtils.enableOrDisableFeatureByRFC(tapEnv, device,
					BroadBandTraceConstants.LOG_MESSAGE_CPU_MEMORY_FRAGMENTATION, true);

			if (status) {
				LOGGER.info("STEP 2: ACTUAL : Successfully updated CPU fragmentation interval using RFC");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}
			LOGGER.info("***************************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			/**
			 * Step 3 : Verify CPU memory fragmentation configuration updated through RFC in
			 * dcmrfc.log file
			 */
			stepNum = "s3";
			errorMessage = "Failed to get the log message in dcmrfc.log file";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info(
					"STEP 3: DESCRIPTION: Verify CPU memory fragmentation configuration updated through RFC in dcmrfc.log file");
			LOGGER.info("STEP 3: ACTION: grep -i CPU_MEMORY_FRAGMENTATION /rdklogs/logs/dcmrfc.log");
			LOGGER.info("STEP 3: EXPECTED: Response should contain the log message in dcmrfc.log file");
			LOGGER.info("******************************************************************************");

			status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandTraceConstants.LOG_MESSAGE_CPU_MEMORY_FRAGMENTATION,
					BroadBandCommandConstants.FILE_DCMRFC_LOG, BroadBandTestConstants.TEN_MINUTE_IN_MILLIS,
					BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
			if (status) {
				LOGGER.info(
						"STEP 3: ACTUAL: Successfully verified the CPU memory fragmentation configuration log message in dcmrfc.log file");
			} else {
				LOGGER.error("STEP 3: ACTUAL: " + errorMessage);
			}
			LOGGER.info("***************************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			/**
			 * Step 4: Verify default CPU Memory fragmentation interval
			 */
			stepNum = "s4";
			errorMessage = "Unable to check default CPU Memory fragmentation interval";
			status = false;
			LOGGER.info("***************************************************************************************");
			LOGGER.info("STEP 4: DESCRIPTION : Verify CPU Memory fragmentation interval changed to 1 hour interval");
			LOGGER.info(
					"STEP 4: ACTION : Execute webpa get command:Device.SelfHeal.X_RDKCENTRAL-COM_CpuMemFragInterval");
			LOGGER.info("STEP 4: EXPECTED : Webpa get operation should get response code as 200 and the value as 1hrs");
			LOGGER.info("***************************************************************************************");

			status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_CPU_FRAGMENTATION_INTERVAL,
					BroadBandTestConstants.STRING_CONSTANT_1, BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
					BroadBandTestConstants.TEN_SECOND_IN_MILLIS);

			if (status) {
				LOGGER.info(
						"STEP 4: ACTUAL : Verified CPU fragmentation interval through Webpa & response is " + response);
			} else {
				LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
			}
			LOGGER.info("***************************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			/**
			 * Step 5 : Verify cron job is scheduled
			 */
			stepNum = "s5";
			errorMessage = "Unable to check  crontab time interval for buddyinfo";
			status = false;
			LOGGER.info("***************************************************************************************");
			LOGGER.info("STEP 5: DESCRIPTION : Verify  cron job is scheduled");
			LOGGER.info("STEP 5: ACTION : Execute command:crontab -l -c /var/spool/cron/crontabs ");
			LOGGER.info("STEP 5: EXPECTED : Response should contain " + "0 1,2,"
					+ "3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23 * * * log_buddyinfo.sh");
			LOGGER.info("***************************************************************************************");
			response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CRONTAB_EXECUTE_BUDDYINFO);
			if (CommonMethods.isNotNull(response)) {
				response = CommonMethods.patternFinder(response,
						BroadBandTestConstants.PATTERN_TO_FETCH_CRONTAB_TIME_INTERVAL_FOR_1HR);
				LOGGER.info("This time interval scheduled in cron tab " + response);
				status = CommonMethods.isNotNull(response)
						&& response.equalsIgnoreCase(BroadBandTestConstants.ONE_HRS_TIME_INTERVAL);
			}
			if (status) {
				LOGGER.info("STEP 5: ACTUAL : Cron job is scheduled for 1 hour CPU fragmentation interval");
			} else {
				LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
			}
			LOGGER.info("***************************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			/**
			 * Step 6 : Verify CPU fragmentation details log message in CPUInfo.txt.0 file
			 */
			stepNum = "s6";
			errorMessage = "Unable to verify CPU fragmentation details log message in CPUInfo.txt.0 file";
			status = false;
			LOGGER.info("***************************************************************************************");
			LOGGER.info("STEP 6: DESCRIPTION : Verify CPU fragmentation details log message in CPUInfo.txt.0 file");
			LOGGER.info("STEP 6: ACTION : Execute command:cat /rdklogs/logs/CPUInfo.txt.0");
			LOGGER.info("STEP 6: EXPECTED : Response should contain the log message" + "Sample output:"
					+ "2019-04-24 [07:32:18] PROC_BUDDYINFO_HOST:CPU_MEM_FRAG-Normal,..."
					+ "2019-04-24 [07:32:18] PROC_BUDDYINFO_PEER:CPU_MEM_FRAG-DMA,..."
					+ "2019-04-24 [07:32:18] PROC_BUDDYINFO_PEER:CPU_MEM_FRAG-Normal,...");
			LOGGER.info("***************************************************************************************");
			try {
				tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_TO_TRIGGER_LOG_BUDDYINFO);
				tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_TO_TRIGGER_LOG_MEM_CPU_INFO);
				response = tapEnv.executeCommandUsingSsh(device,
						BroadBandCommandConstants.CMD_TO_GET_PROCESS_MEM_LOG_COUNT);
				if (CommonMethods.isNotNull(response)
						&& (!CommonMethods.patternMatcher(response, BroadBandTestConstants.STRING_VALUE_12))) {
					tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_TO_SET_PROCESS_MEM_LOG_COUNT);
					response = tapEnv.executeCommandUsingSsh(device,
							BroadBandCommandConstants.CMD_TO_GET_PROCESS_MEM_LOG_COUNT);
					if (CommonMethods.isNotNull(response)
							&& CommonMethods.patternMatcher(response, BroadBandTestConstants.STRING_VALUE_12)) {
						tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);
						response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_TO_GET_CPU_INFO);
					} else {
						errorMessage = "Unable to set process_memory_log_count to 12 ";
					}
				}
			} catch (Exception e) {
				errorMessage = errorMessage + e.getMessage();
				LOGGER.error("Exception occured during execution" + errorMessage);
			}
			if (CommonMethods.isNotNull(response)) {
				arrPrimaryZones = CommonMethods.patternFinderToReturnAllMatchedString(response,
						BroadBandTestConstants.PATTERN_TO_FETCH_HOST_ZONES_CPU_MEMFRAG);
				arrPrimaryValues = CommonMethods.patternFinderToReturnAllMatchedString(response,
						BroadBandTestConstants.PATTERN_TO_FETCH_HOST_VALUES_CPU_MEMFRAG);
				if ((arrPrimaryZones == null || arrPrimaryZones.isEmpty()) && arrPrimaryValues == null
						|| (arrPrimaryValues.isEmpty())) {

					LOGGER.info("HOST memory does not have any zones and values");
				} else {
					LOGGER.info("These are the zones present in CPUInfo.txt.0 for HOST : " + arrPrimaryZones);
					LOGGER.info("These are the Values present in CPUInfo.txt.0 for HOST : " + arrPrimaryValues);
					status = true;
				}
			} else {
				LOGGER.info("CPUInfo.txt.0 doesnot have log for HOST memory");
			}
			if (CommonMethods.isAtomSyncAvailable(device, tapEnv) && status) {
				status = false;
				if (CommonMethods.isNotNull(response)) {
					arrSecondaryZones = CommonMethods.patternFinderToReturnAllMatchedString(response,
							BroadBandTestConstants.PATTERN_TO_FETCH_PEER_ZONES_CPU_MEMFRAG);
					arrSecondaryValues = CommonMethods.patternFinderToReturnAllMatchedString(response,
							BroadBandTestConstants.PATTERN_TO_FETCH_PEER_VALUES_CPU_MEMFRAG);
					if ((arrSecondaryZones == null || arrSecondaryZones.isEmpty()) && arrSecondaryValues == null
							|| (arrSecondaryValues.isEmpty())) {
						LOGGER.info("PEER memory does not have any zones and values");
					} else {
						LOGGER.info("These are the zones present in CPUInfo.txt.0 for PEER : " + arrSecondaryZones);

						LOGGER.info("These are the Values present in CPUInfo.txt.0 for PEER : " + arrSecondaryValues);
						status = true;
					}
				} else {
					LOGGER.info("CPUInfo.txt.0 doesnot have log for PEER memory");
				}
			} else {
				LOGGER.info("It is only applicable for Atom based devices");
			}
			if (status) {
				LOGGER.info("STEP 6: ACTUAL : Verified CPU fragmentation log message in CPUInfo.txt.0 file");
			} else {
				LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
			}
			LOGGER.info("***************************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			/**
			 * Step 7 : Verify Log message format in CPUInfo.txt.0 file
			 */
			stepNum = "s7";
			errorMessage = "Unable verify Log message format in CPUInfo.txt.0 file";
			status = false;
			LOGGER.info("***************************************************************************************");
			LOGGER.info("STEP 7: DESCRIPTION : Verify Log message format in CPUInfo.txt.0 file");
			LOGGER.info("STEP 7: ACTION : Execute command:cat /rdklogs/logs/CPUInfo.txt.0");
			LOGGER.info("STEP 7: EXPECTED : Log message format should be Timestamp:CPU Mem Frag:zone:value");
			LOGGER.info("***************************************************************************************");
			if (CommonMethods.isNotNull(response)) {
				status = CommonMethods.patternMatcher(response,
						BroadBandTestConstants.PATTERN_TO_CHECK_CPU_FRAGMENTATION_FORMAT);
			}
			if (status) {
				LOGGER.info("STEP 7: ACTUAL : Log messages are present in correct format in CPUInfo.txt.0 file");
			} else {
				LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
			}
			LOGGER.info("***************************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			/**
			 * Step8 : Get CPU fragmentation details from /proc/buddyinfo
			 */
			stepNum = "s8";
			errorMessage = "Unable to fetch details from buddyinfo";
			status = false;
			LOGGER.info("***************************************************************************************");
			LOGGER.info("STEP 8: DESCRIPTION : Get CPU fragmentation details from /proc/buddyinfo");
			LOGGER.info("STEP 8: ACTION : Execute command:cat /proc/buddyinfo");
			LOGGER.info("STEP 8: EXPECTED : Response should contain the CPU fragmentation details" + "Sample output:"
					+ "~ # cat /proc/buddyinfo"
					+ "Node 0, zone   Normal      9     24     86     36      6      3      2      2      2      3     13");
			LOGGER.info("***************************************************************************************");
			response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_TO_GET_BUDDYINFO);
			if (CommonMethods.isNotNull(response)) {
				arrBuddyInfo = CommonMethods.patternFinderToReturnAllMatchedString(response,
						BroadBandTestConstants.PATTERN_TO_FETCH_BUDDYINFO);
				LOGGER.info("These are the logs present in buddyinfo: " + arrBuddyInfo);
				status = BroadBandCommonUtils.convertListOfStringsToLowerCase(arrBuddyInfo)
						.equals(BroadBandCommonUtils.convertListOfStringsToLowerCase(arrPrimaryZones));
				LOGGER.info("CPUInfo.txt.0 and buddyinfo having same zones for host");

			} else {
				LOGGER.info("CPU fragmentation details is not present for host memory in buddyinfo");
			}
			if (CommonMethods.isAtomSyncAvailable(device, tapEnv) && status) {
				status = false;
				response = tapEnv.executeCommandOnAtom(device, BroadBandCommandConstants.CMD_TO_GET_BUDDYINFO);
				if (CommonMethods.isNotNull(response)) {
					arrBuddyInfoAtomDevice = CommonMethods.patternFinderToReturnAllMatchedString(response,
							BroadBandTestConstants.PATTERN_TO_FETCH_BUDDYINFO);
					LOGGER.info("These are the logs present in buddyinfo: " + arrBuddyInfoAtomDevice);
					status = (arrBuddyInfoAtomDevice.equals(arrSecondaryZones));
					LOGGER.info("CPUInfo.txt.0 and buddyinfo having same zones for peer");

				} else {
					LOGGER.info("CPU fragmentation details is not present for peer memory in buddyinfo");
				}
			} else {
				LOGGER.info("It is only applicable for Atom based devices");
			}
			if (status) {
				LOGGER.info("STEP 8: ACTUAL : CPU Fragmentation details are present in /proc/buddyinfo");
			} else {
				LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
			}
			LOGGER.info("***************************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			/**
			 * Step 9 : Verify default primary and Secondary CPU fragmentation zone values
			 * using webpa
			 */
			stepNum = "s9";
			errorMessage = "Unable to verify default primary and secondary CPU fragmentation zone values using webpa";
			status = false;
			LOGGER.info("***************************************************************************************");
			LOGGER.info(
					"STEP 9: DESCRIPTION : Verify default primary and Secondary CPU fragmentation zone values using webpa");
			LOGGER.info(
					"STEP 9: ACTION : Execute webpa get command:Device.SelfHeal.CpuMemFrag.1.DMA, Device.SelfHeal.CpuMemFrag.1.DMA32, Device.SelfHeal.CpuMemFrag.1.Normal, and Device.CpuMemFrag.1.Highmem,Device.SelfHeal.CpuMemFrag.2.DMA, Device.SelfHeal.CpuMemFrag.2.DMA32, Device.SelfHeal.CpuMemFrag.2.Normal, and Device.CpuMemFrag.2.Highmem");
			LOGGER.info(
					"STEP 9: EXPECTED : Response should contain the primary and secondary CPU memory fragmentation details");
			LOGGER.info("***************************************************************************************");

			Map<String, String> webpaResponseMap = tapEnv.executeMultipleWebPaGetCommands(device,
					arrExecuteMultiplePrimaryParams);
			LOGGER.info("Compare the values in CPUInfo.txt.0 " + arrPrimaryValues
					+ " and cpu memory fragmentation details through webpa parameter " + webpaResponseMap);
			for (String param : webpaResponseMap.values()) {
				if (arrPrimaryValues.contains(webpaResponseMap.get(param))
						|| CommonMethods.isNull(webpaResponseMap.get(param))) {
					status = true;
				} else {
					status = false;
					break;
				}
			}
			if (CommonMethods.isAtomSyncAvailable(device, tapEnv) && status) {
				status = false;
				webpaResponseMap = tapEnv.executeMultipleWebPaGetCommands(device, arrExecuteMultipleSecondaryParams);
				LOGGER.info("Compare the values in CPUInfo.txt.0 " + arrSecondaryValues
						+ " and cpu memory fragmentation details through webpa parameter " + webpaResponseMap);
				for (String param : webpaResponseMap.values()) {
					if (arrSecondaryValues.contains(webpaResponseMap.get(param))
							|| CommonMethods.isNull(webpaResponseMap.get(param))) {

						status = true;
					} else {
						status = false;
						break;
					}
				}
			} else {
				LOGGER.info("Its is applicable for AtomSyncDevices devices only");
			}
			if (status) {
				LOGGER.info(
						"STEP 9: ACTUAL : Verified primary and secondary CPU fragmentation zone values using webpa");
			} else {
				LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
			}
			LOGGER.info("***************************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, false, errorMessage,
					false);
		} finally {
			status = false;
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS");
			/**
			 * Post-Condition 1 : Set CPU fragmentation interval to default
			 */
			LOGGER.info("###########################################################################");
			LOGGER.info("POST-CONDITION " + postConditionStepNum
					+ ": DESCRIPTION : Set CPU fragmentation interval to default and Removing RFC profile from Proxy xconf dcm server");
			LOGGER.info("POST-CONDITION " + postConditionStepNum
					+ " : ACTION : Device.SelfHeal.X_RDKCENTRAL-COM_CpuMemFragInterval");
			LOGGER.info("POST-CONDITION " + postConditionStepNum
					+ " : EXPECTED : Webpa parameter should set CPU fragmentation interval to default and RFC profile should be removed");
			LOGGER.info("###########################################################################");

			defaultValue = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_CPU_FRAGMENTATION_INTERVAL, BroadBandTestConstants.CONSTANT_2,
					BroadBandTestConstants.STRING_CONSTANT_4);
			removingRfcProfile = (HttpStatus.SC_OK == BroadBandRfcFeatureControlUtils
					.clearSettingsInProxyXconfDcmServerForRDKB(device, tapEnv, false,
							BroadBandTraceConstants.LOG_MESSAGE_CPU_MEMORY_FRAGMENTATION));
			status = defaultValue && removingRfcProfile;

			if (status) {
				LOGGER.info("POST-CONDITION " + postConditionStepNum
						+ " : ACTUAL : Successfully changed CPU fragmentation interval to default and Removed RFC profile Proxy xconf dcm server");
			} else {
				LOGGER.error("POST-CONDITION " + postConditionStepNum + " : ACTUAL : "
						+ (defaultValue ? "" : " Unable to change CPU fragmentation interval to default")
						+ (removingRfcProfile ? "" : "RFC profile is not removed from Proxy xconf dcm server"));
			}
			LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
			/**
			 * Post-Condition 2 : BEGIN BROAD BAND DEVICE REACTIVATION
			 */
			if (isFactoryReset) {
				++postConditionStepNum;
				LOGGER.info("###########################################################################");
				LOGGER.info("POST-CONDITION " + postConditionStepNum
						+ " : DESCRIPTION : BEGIN BROAD BAND DEVICE REACTIVATION.");
				LOGGER.info("POST-CONDITION " + postConditionStepNum + " : ACTION : BROAD BAND DEVICE REACTIVATION. ");
				LOGGER.info("POST-CONDITION " + postConditionStepNum + " : EXPECTED : device should get reactivated");
				LOGGER.info("###########################################################################");
				BroadBandWiFiUtils.reactivateDeviceUsingWebPa(tapEnv, device);
			}
			/**
			 * Post-Condition 3 : Revert periodic firmware check process to deafult value
			 */
			if (postExecution) {
				++postConditionStepNum;
				LOGGER.info("#####################################################################################");
				LOGGER.info("POST-CONDITION " + postConditionStepNum
						+ ": DESCRIPTION : Revert periodic firmware check process to deafult value");
				LOGGER.info("POST-CONDITION " + postConditionStepNum
						+ ": ACTION : Execute webpa command to set finger print enable parameter to true");
				LOGGER.info(
						"POST-CONDITION " + postConditionStepNum + ": EXPECTED : Post condition executed successfully");
				LOGGER.info("#####################################################################################");
				status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_PERIODIC_FW_CHECK_ENABLE, BroadBandTestConstants.CONSTANT_3,
						BroadBandTestConstants.FALSE, BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
						BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);

				if (status) {
					LOGGER.info("POST-CONDITION " + postConditionStepNum
							+ ": ACTUAL : Post condition executed successfully");
				} else {
					LOGGER.error("POST-CONDITION " + postConditionStepNum + ": ACTUAL : Post condition failed");
				}
			}
			LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);

			LOGGER.info("################### ENDING POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-TELEMETRY-1006");
	}

	/**
	 * Verify configurable telemetry tag functionality
	 * <li>Verify default value of UniqueTelemetryId Enable parameter</li>
	 * <li>Verify default value of UniqueTelemetryId TagString parameter</li>
	 * <li>Verify default value of UniqueTelemetryId TimingInterval parameter</li>
	 * <li>Copy and update dcm.properties with mock server url for
	 * DCM_LOG_SERVER_URL and DCM_RFC_SERVER_URL
	 * <li>
	 * <li>Post the payload data for test telemetry marker</li>
	 * <li>Post RFC config data to set UniqueTelemetryId parameters</li>
	 * <li>Retrieve timestamp from the device and reboot</li>
	 * <li>Verify UniqueTelemetryId TagString parameter value is updated with posted
	 * RFC data</li>
	 * <li>Verify UniqueTelemetryId TimingInterval parameter value is updated with
	 * posted RFC data</li>
	 * <li>Verify UniqueTelemetryId Enable parameter value is updated with posted
	 * RFC data</li>
	 * <li>Verify configured tag string is printed in PAMlog</li>
	 * <li>Verify telemetry logging for configured tag in dcmscript.log with
	 * timestamp later than timestamp obtained in step 7</li>
	 * <li>Reset values of UniqueTelemetryId parameters to default</li>
	 * <li>Clear Arm/Consolelog, dcmscript.log and obtain device timestamp</li>
	 * <li>Set value of UniqueTelemetryId TagString parameter using tr69</li>
	 * <li>Set value of UniqueTelemetryId TimingInterval parameter using tr69</li>
	 * <li>Set value of UniqueTelemetryId Enable parameter using tr69</li>
	 * <li>Verify configured tag string is printed in Arm/Consolelog</li>
	 * <li>Verify telemetry logging for configured tag in dcmscript.log with
	 * timestamp later than timestamp obtained in step 14</li>
	 * <li>Reset values of UniqueTelemetryId parameters to default</li>
	 * <li>Clear Arm/Consolelog, dcmscript.log and obtain device timestamp</li>
	 * <li>Set value of UniqueTelemetryId TagString parameter using webpa</li>
	 * <li>Set value of UniqueTelemetryId TimingInterval parameter using webpa</li>
	 * <li>Set value of UniqueTelemetryId Enable parameter using webpa</li>
	 * <li>Verify configured tag string is printed in PAMlog</li>
	 * <li>Clear log file and verify configured tag string is printed again after
	 * configured timing interval period</li>
	 * <li>Verify telemetry logging for configured tag in dcmscript.log with
	 * timestamp later than timestamp obtained in step 19</li>
	 * <li>Set value of UniqueTelemetryId TagString parameter with value greater
	 * than 255 characters</li>
	 * <li>Verify invalid string is not logged in Arm/Consolelog</li>
	 * <li>Reset values of UniqueTelemetryId parameters to default</li>
	 * 
	 * @param device {@link Dut}
	 * 
	 * @author Ashwin Sankara
	 * 
	 * @Refactor Sruthi Santhosh
	 * 
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = BroadBandTestGroup.TELEMETRY)
	@TestDetails(testUID = "TC-RDKB-TELEMETRY-1003")
	public void testVerifyConfigurableTelemetryTag(Dut device) {

		String testCaseId = "TC-RDKB-TELEMETRY-103";
		String step = "s1";
		String errorMessage = null;
		boolean result = false;
		String response = null;
		String deviceTimeStamp = null;
		long startTime = BroadBandTestConstants.CONSTANT_0;
		boolean isTelemetryEnabled = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
				BroadBandWebPaConstants.WEBPA_PARAM_FOR_TELEMETRY_2_0_ENABLE, BroadBandTestConstants.TRUE,
				BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS, BroadBandTestConstants.TEN_SECOND_IN_MILLIS);

		BroadBandResultObject resultObject = new BroadBandResultObject();

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-TELEMETRY-1003");
		LOGGER.info("TEST DESCRIPTION: Verify configurable telemetry tag functionality");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Verify default value of UniqueTelemetryId Enable parameter");
		LOGGER.info("2. Verify default value of UniqueTelemetryId TagString parameter");
		LOGGER.info("3. Verify default value of UniqueTelemetryId TimingInterval parameter");
		LOGGER.info("4. Copy and update dcm.properties with mock server url for DCM_LOG_SERVER_URL"
				+ " and DCM_RFC_SERVER_URL");
		LOGGER.info("5. Post the payload data for test telemetry marker");
		LOGGER.info("6. Post RFC config data to set UniqueTelemetryId parameters");
		LOGGER.info("7. Retrieve timestamp from the device and reboot");
		LOGGER.info("8. Verify UniqueTelemetryId TagString parameter value is updated with posted RFC data");
		LOGGER.info("9. Verify UniqueTelemetryId TimingInterval parameter value is updated with posted RFC data");
		LOGGER.info("10. Verify UniqueTelemetryId Enable parameter value is updated with posted RFC data");
		LOGGER.info("11. Verify configured tag string is printed in PAMlog");
		LOGGER.info("12. Verify telemetry logging for configured tag in dcmscript.log with timestamp"
				+ " later than timestamp obtained in step 7");
		LOGGER.info("13. Reset values of UniqueTelemetryId parameters to default");
		LOGGER.info("14. Clear Arm/Consolelog, dcmscript.log and obtain device timestamp");
		LOGGER.info("15. Set value of UniqueTelemetryId TagString parameter using tr69");
		LOGGER.info("16. Set value of UniqueTelemetryId TimingInterval parameter using tr69");
		LOGGER.info("17. Set value of UniqueTelemetryId Enable parameter using tr69");
		LOGGER.info("18. Verify configured tag string is printed in Arm/Consolelog");
		LOGGER.info("19. Verify telemetry logging for configured tag in dcmscript.log with timestamp"
				+ " later than timestamp obtained in step 14");
		LOGGER.info("20. Reset values of UniqueTelemetryId parameters to default");
		LOGGER.info("21. Clear Arm/Consolelog, dcmscript.log and obtain device timestamp");
		LOGGER.info("22. Set value of UniqueTelemetryId TagString parameter using webpa");
		LOGGER.info("23. Set value of UniqueTelemetryId TimingInterval parameter using webpa");
		LOGGER.info("24. Set value of UniqueTelemetryId Enable parameter using webpa");
		LOGGER.info("25. Verify configured tag string is printed in PAMlog");
		LOGGER.info("26. Clear log file and verify configured tag string is printed again"
				+ " after configured timing interval period");
		LOGGER.info("27. Verify telemetry logging for configured tag in dcmscript.log with timestamp"
				+ " later than timestamp obtained in step 19");
		LOGGER.info("28. Set value of UniqueTelemetryId TagString parameter with value greater than 255 characters");
		LOGGER.info("29. Verify invalid string is not logged in Arm/Consolelog");
		LOGGER.info("30. Reset values of UniqueTelemetryId parameters to default");
		LOGGER.info("#######################################################################################");

		try {

			step = "s1";
			errorMessage = "Default value of UniqueTelemetryId Enable parameter is not false";
			result = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION : Verify default value of UniqueTelemetryId Enable parameter");
			LOGGER.info("STEP 1: ACTION : Execute webpa command to get value of "
					+ "Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.UniqueTelemetryId.Enable");
			LOGGER.info("STEP 1: EXPECTED : WebPA request successful and value is false");
			LOGGER.info("**********************************************************************************");
			response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_UNIQUE_TELEMETRY_ENABLE);
			result = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(BroadBandTestConstants.FALSE);

			if (result) {
				LOGGER.info("STEP 1: ACTUAL :WebPA request successful and value is false");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

			step = "s2";
			errorMessage = "Default value of UniqueTelemetryId TagString parameter is not null";
			result = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION : Verify default value of UniqueTelemetryId TagString parameter");
			LOGGER.info("STEP 2: ACTION : Execute webpa command to get value of "
					+ "Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.UniqueTelemetryId.TagString");
			LOGGER.info("STEP 2: EXPECTED : WebPA request successful and value is null");
			LOGGER.info("**********************************************************************************");
			result = CommonMethods.isNull(
					tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_UNIQUE_TELEMETRY_TAGSTRING));

			if (result) {
				LOGGER.info("STEP 2: ACTUAL :WebPA request successful and value is null");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

			step = "s3";
			errorMessage = "Default value of UniqueTelemetryId TimingInterval parameter is not 0";
			result = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 3: DESCRIPTION : Verify default value of UniqueTelemetryId TimingInterval parameter");
			LOGGER.info("STEP 3: ACTION : Execute webpa command to get value of "
					+ "Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.UniqueTelemetryId.TimingInterval");
			LOGGER.info("STEP 3: EXPECTED : WebPA request successful and value is 0");
			LOGGER.info("**********************************************************************************");
			response = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_UNIQUE_TELEMETRY_TIMINGINTERVAL);
			result = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(BroadBandTestConstants.STRING_ZERO);

			if (result) {
				LOGGER.info("STEP 3: ACTUAL :WebPA request successful and value is 0");
			} else {
				LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

			step = "s4";
			errorMessage = "Unable to override value of DCM_LOG_SERVER_URL in /nvram/dcm.properties";
			result = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 4: DESCRIPTION : Copy and update dcm.properties with mock server url"
					+ " for DCM_LOG_SERVER_URL and DCM_RFC_SERVER_URL");
			LOGGER.info("STEP 4: ACTION : Execute copy and sed commands to override DCM_LOG_SERVER_URL"
					+ " and DCM_RFC_SERVER_URL");
			LOGGER.info("STEP 4: EXPECTED : Values overridden in nvram successfully");
			LOGGER.info("**********************************************************************************");
			if (BroadBandTestConstants.CONSTANT_1 == BroadBandTelemetryUtils.copyAndUpdateDcmProperties(device,
					tapEnv)) {
				errorMessage = "Unable to copy rfc.properties from /etc to /nvram";
				if (BroadBandRfcFeatureControlUtils.copyRfcPropertiesFromEtcToNVRAM(device, tapEnv)) {
					errorMessage = "Unable to override value of DCM_RFC_SERVER_URL in /nvram/dcm.properties";
					result = BroadBandRfcFeatureControlUtils.copyAndUpdateRfcPropertiesNewXconfUrl(device, tapEnv,
							BroadbandPropertyFileHandler.getProxyXconfRfcUrl());
				}
			}
			if (result) {
				LOGGER.info("STEP 4: ACTUAL :Values overridden in nvram successfully");
			} else {
				LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
			}

			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, true);

			step = "s5";
			errorMessage = "Unable to post payload data for telemetry marker successfully";
			result = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 5: DESCRIPTION : Post the payload data for test telemetry marker");
			LOGGER.info("STEP 5: ACTION : Post telemetry profile with content Test_Telemetry_Tag"
					+ " in ArmConsolelog and Consolelog");
			LOGGER.info("STEP 5: EXPECTED : Telemetry data posted successfully with http 200 response");
			LOGGER.info("**********************************************************************************");
			result = BroadBandTestConstants.CONSTANT_200 == BroadBandTelemetryUtils.postDataToProxyDcmServer(device,
					tapEnv, false, true);
			if (result) {
				LOGGER.info("STEP 5: ACTUAL :Telemetry data posted successfully with http 200 response");
			} else {
				LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
			}

			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, true);

			step = "s6";
			errorMessage = "Unable to post payload data for RFC configuration successfully";
			result = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 6: DESCRIPTION : Post RFC config data to set UniqueTelemetryId parameters");
			LOGGER.info("STEP 6: ACTION : Post RFC feature to enable UniqueTelemetryId logging parameters");
			LOGGER.info("STEP 6: EXPECTED : RFC data posted successfully with http 200 response");
			LOGGER.info("**********************************************************************************");
			result = BroadBandRfcFeatureControlUtils.enableOrDisableFeatureInProxyXconf(tapEnv, device,
					BroadBandTestConstants.CONFIGURABLE_TELEMETRY, true);
			if (result) {
				LOGGER.info("STEP 6: ACTUAL :RFC data posted successfully with http 200 response");
			} else {
				LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
			}

			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, true);

//			step = "s7";
//			errorMessage = "Unable to retrieve timestamp from the device";
//			result = false;
//			LOGGER.info("**********************************************************************************");
//			LOGGER.info("STEP 7: DESCRIPTION : Retrieve timestamp from the device and reboot");
//			LOGGER.info("STEP 7: ACTION : Execute command: date +%y%m%d-%H:%M:%S and reboot the device");
//			LOGGER.info("STEP 7: EXPECTED : Obtained timestamp from device and rebooted successfully");
//			LOGGER.info("**********************************************************************************");
//
//			deviceTimeStamp = BroadBandCommonUtils.getCurrentTimeStampOnDevice(tapEnv, device);
//			if (CommonMethods.isNotNull(deviceTimeStamp)) {
//				errorMessage = "Unable to reboot the device";
//				result = BroadBandTelemetryUtils.rebootAndWaitForDeviceAccessible(tapEnv, device,
//						tapEnv.getWaitValue(device, AVConstants.PROP_KEY_WAIT_AFTER_HARD_REBOOT_INITIATED));
//			}
//			if (result) {
//				LOGGER.info("STEP 7: ACTUAL :Obtained timestamp from device and rebooted successfully");
//			} else {
//				LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
//			}
//
//			LOGGER.info("**********************************************************************************");
//			tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, true);

			step = "s8";
			result = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 8: DESCRIPTION : Verify UniqueTelemetryId TagString parameter value"
					+ " is updated with posted RFC data");
			LOGGER.info("STEP 8: ACTION : Check rfcresponse.json, rfc_configdata, dcmrfclog and parameter value");
			LOGGER.info("STEP 8: EXPECTED : Value of  UniqueTelemetryId TagString parameter value"
					+ " is Test_Telemetry_Tag");
			LOGGER.info("**********************************************************************************");
			resultObject = BroadBandRfcFeatureControlUtils.verifyParameterUpdatedByRfc(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_UNIQUE_TELEMETRY_TAGSTRING,
					BroadBandTestConstants.TELEMETRY_MARKER_TEST_TELEMETRY_TAG);
			errorMessage = resultObject.getErrorMessage();
			result = resultObject.isStatus();
			if (result) {
				LOGGER.info(
						"STEP 8: ACTUAL :Value of  UniqueTelemetryId TagString parameter value is Test_Telemetry_Tag");
			} else {
				LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
			}

			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

			step = "s9";
			result = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 9: DESCRIPTION : Verify UniqueTelemetryId TimingInterval parameter value"
					+ " is updated with posted RFC data");
			LOGGER.info("STEP 9: ACTION : Check rfcresponse.json, rfc_configdata, dcmrfclog and parameter value");
			LOGGER.info("STEP 9: EXPECTED : Value of  UniqueTelemetryId TimingInterval parameter value is 15");
			LOGGER.info("**********************************************************************************");
			resultObject = BroadBandRfcFeatureControlUtils.verifyParameterUpdatedByRfc(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_UNIQUE_TELEMETRY_TIMINGINTERVAL,
					BroadBandTestConstants.STRING_VALUE_FIFTEEN);
			errorMessage = resultObject.getErrorMessage();
			result = resultObject.isStatus();
			if (result) {
				LOGGER.info("STEP 9: ACTUAL :Value of  UniqueTelemetryId TimingInterval parameter value is 15");
			} else {
				LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
			}

			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

			step = "s10";
			result = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 10: DESCRIPTION : Verify UniqueTelemetryId Enable parameter value"
					+ " is updated with posted RFC data");
			LOGGER.info("STEP 10: ACTION : Check rfcresponse.json, rfc_configdata, dcmrfclog and parameter value");
			LOGGER.info("STEP 10: EXPECTED : Value of  UniqueTelemetryId Enable parameter value is true");
			LOGGER.info("**********************************************************************************");
			resultObject = BroadBandRfcFeatureControlUtils.verifyParameterUpdatedByRfc(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_UNIQUE_TELEMETRY_ENABLE, BroadBandTestConstants.TRUE);
			errorMessage = resultObject.getErrorMessage();
			result = resultObject.isStatus();
			if (result) {
				LOGGER.info("STEP 10: ACTUAL :Value of  UniqueTelemetryId Enable parameter value is true");
			} else {
				LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
			}

			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

			step = "s11";
			result = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 11: DESCRIPTION : Verify configured tag string is printed in PAMlog");
			LOGGER.info("STEP 11: ACTION : Execute command: grep \"Test_Telemetry_Tag\" /rdklogs/logs/PAMlog.txt.0");
			LOGGER.info("STEP 11: EXPECTED : Configured log message is printed in PAMlog");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to find configured tag string printed in PAMlog";
			startTime = System.currentTimeMillis();
			do {
				response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
						BroadBandTestConstants.TELEMETRY_MARKER_TEST_TELEMETRY_TAG,
						BroadBandTestConstants.COMMAND_NTP_LOG_FILE, BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
						BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
				result = CommonMethods.isNotNull(response) && CommonMethods.patternMatcher(response,
						BroadBandTestConstants.TELEMETRY_MARKER_TEST_TELEMETRY_TAG);
			} while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.FIFTEEN_MINUTES_IN_MILLIS
					&& !result
					&& BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS));
			if (result) {
				LOGGER.info("STEP 11: ACTUAL :Configured log message is printed in PAMlog");
			} else {
				LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
			}

			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

			step = "s12";
			result = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 12: DESCRIPTION : Verify telemetry logging for configured tag in dcmscript.log"
					+ " with timestamp later than timestamp obtained in step 7");
			LOGGER.info("STEP 12: ACTION : Execute command: grep \"Test_Telemetry_Tag\" /rdklogs/logs/dcmscript.log");
			LOGGER.info("STEP 12: EXPECTED : Telemetry log message is present for configured tag string with timestamp"
					+ " later than timestamp in step 7");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to find telemetry logging in dcmscript log file for configured telemetry marker";
			startTime = System.currentTimeMillis();
			if (!isTelemetryEnabled) {
				do {
					result = BroadBandCommonUtils.verifyConsoleLogByPassingDateFormat(tapEnv, device,
							BroadBandTestConstants.TELEMETRY_MARKER_TEST_TELEMETRY_TAG,
							BroadBandTestConstants.DCMSCRIPT_LOG_FILE, deviceTimeStamp,
							BroadBandTestConstants.PATTERN_MATCHER_TIMESTAMP_SPEEDTEST_LOG_MESSAGE,
							BroadBandTestConstants.TIMESTAMP_FORMAT_SPEEDTEST_LOG_MESSAGE);
				} while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS
						&& !result && BroadBandCommonUtils.hasWaitForDuration(tapEnv,
								BroadBandTestConstants.ONE_MINUTE_IN_MILLIS));
				if (result) {
					LOGGER.info(
							"STEP 12: ACTUAL : Telemetry log message is present for configured tag string with timestamp\"\r\n"
									+ "					+ \" later than timestamp in step 7\"");
				} else {
					LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
				}

				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);
			} else {

				errorMessage = "Test step not applicable when for telemtry 2.0 ";
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);

			}

			if (DeviceModeHandler.isBusinessClassDevice(device)) {

				LOGGER.info("Steps 13 to 19 are not applicable for Business Class and Fiber Devices as TR69 is not present in BCI devices");
				LOGGER.info("#######################################################################################");
				tapEnv.updateExecutionForAllStatus(device, testCaseId, "s13", ExecutionStatus.NOT_APPLICABLE,
						"N/A for Business class devices", false);
				tapEnv.updateExecutionForAllStatus(device, testCaseId, "s14", ExecutionStatus.NOT_APPLICABLE,
						"N/A for Business class devices", false);
				tapEnv.updateExecutionForAllStatus(device, testCaseId, "s15", ExecutionStatus.NOT_APPLICABLE,
						"N/A for Business class devices", false);
				tapEnv.updateExecutionForAllStatus(device, testCaseId, "s16", ExecutionStatus.NOT_APPLICABLE,
						"N/A for Business class devices", false);
				tapEnv.updateExecutionForAllStatus(device, testCaseId, "s17", ExecutionStatus.NOT_APPLICABLE,
						"N/A for Business class devices", false);
				tapEnv.updateExecutionForAllStatus(device, testCaseId, "s18", ExecutionStatus.NOT_APPLICABLE,
						"N/A for Business class devices", false);
				tapEnv.updateExecutionForAllStatus(device, testCaseId, "s19", ExecutionStatus.NOT_APPLICABLE,
						"N/A for Business class devices", false);
			} else {

				step = "s13";
				result = false;
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP 13: DESCRIPTION : Reset values of UniqueTelemetryId parameters to default");
				LOGGER.info("STEP 13: ACTION : Reset values of UniqueTelemetryId parameters using webpa"
						+ " or dmcli to false, 0 and null");
				LOGGER.info("STEP 13: EXPECTED : Parameter values reset successfully");
				LOGGER.info("**********************************************************************************");
				errorMessage = "Unable to reset values of UniqueTelemetryId parameters to default successfully";
				result = BroadBandTelemetryUtils.resetDefaultConfigurableTelemetryParameters(device, tapEnv);
				if (result) {
					LOGGER.info("STEP 13: ACTUAL : Parameter values reset successfully");
				} else {
					LOGGER.error("STEP 13: ACTUAL : " + errorMessage);
				}

				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

				step = "s14";
				result = false;
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP 14: DESCRIPTION : Clear Arm/Consolelog, dcmscript.log and obtain device timestamp");
				LOGGER.info("STEP 14: ACTION : Execute echo clear commands for Arm/Consolelog, dcmscript.log"
						+ " and date command for device timestamp");
				LOGGER.info("STEP 14: EXPECTED : Cleared log files and obtained timestamp successfully");
				LOGGER.info("**********************************************************************************");
				errorMessage = BroadBandCommonUtils.concatStringUsingStringBuffer("Unable to clear ",
						CommonMethods.isAtomSyncAvailable(device, tapEnv) ? BroadBandCommandConstants.FILE_ARMCONSOLELOG
								: BroadBandCommandConstants.FILE_CONSOLELOG,
						" file successfully");
				if (CommonUtils.clearLogFile(tapEnv, device,
						CommonMethods.isAtomSyncAvailable(device, tapEnv) ? BroadBandCommandConstants.FILE_ARMCONSOLELOG
								: BroadBandCommandConstants.FILE_CONSOLELOG)) {
					errorMessage = "Unable to clear dcmscript.log file successfully";
					if (CommonUtils.clearLogFile(tapEnv, device, BroadBandCommandConstants.FILE_DCMRFC_LOG)) {
						errorMessage = "Unable to retrieve timestamp from the device";
						deviceTimeStamp = BroadBandCommonUtils.getCurrentTimeStampOnDevice(tapEnv, device);
						result = CommonMethods.isNotNull(deviceTimeStamp);
					}
				}
				if (result) {
					LOGGER.info("STEP 14: ACTUAL : Cleared log files and obtained timestamp successfully");
				} else {
					LOGGER.error("STEP 14: ACTUAL : " + errorMessage);
				}

				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);
				if (DeviceModeHandler.isDSLDevice(device)) {
					LOGGER.info("Steps 15 to 19 are not applicable for dsl as TR69 is not present ");
					LOGGER.info(
							"#######################################################################################");

					tapEnv.updateExecutionForAllStatus(device, testCaseId, "s15", ExecutionStatus.NOT_APPLICABLE,
							"TR069 is not supported on the dsl device", false);
					tapEnv.updateExecutionForAllStatus(device, testCaseId, "s16", ExecutionStatus.NOT_APPLICABLE,
							" TR069 is not supported on the dsl device", false);
					tapEnv.updateExecutionForAllStatus(device, testCaseId, "s17", ExecutionStatus.NOT_APPLICABLE,
							"TR069 is not supported on the dsl device", false);
					tapEnv.updateExecutionForAllStatus(device, testCaseId, "s18", ExecutionStatus.NOT_APPLICABLE,
							"TR069 is not supported on the dsl device", false);
					tapEnv.updateExecutionForAllStatus(device, testCaseId, "s19", ExecutionStatus.NOT_APPLICABLE,
							"TR069 is not supported on the dsl device", false);
				} else {

					step = "s15";
					result = false;
					LOGGER.info("**********************************************************************************");
					LOGGER.info("STEP 15: DESCRIPTION : Set value of UniqueTelemetryId TagString parameter using tr69");
					LOGGER.info("STEP 15: ACTION : Execute tr69 command to set value of "
							+ "Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.UniqueTelemetryId.TagString to Test_Telemetry_Tag");
					LOGGER.info(
							"STEP 15: EXPECTED : Tr69 set request success and parameter value is Test_Telemetry_Tag");
					LOGGER.info("**********************************************************************************");
					try {
						errorMessage = "Unable to set value of UniqueTelemetryId TagString parameter using tr69";

						Parameter setParam = new Parameter();

						setParam.setDataType(TR69ParamDataType.STRING.get());
						setParam.setParamName(BroadBandWebPaConstants.WEBPA_PARAM_UNIQUE_TELEMETRY_TAGSTRING);
						setParam.setParamValue(BroadBandTestConstants.TELEMETRY_MARKER_TEST_TELEMETRY_TAG);
						List<Parameter> parameters = new ArrayList<Parameter>();
						parameters.add(setParam);
						response = tapEnv.setTR69ParameterValues(device, parameters);
						result = CommonMethods.isNotNull(response)
								&& response.equalsIgnoreCase(AutomaticsConstants.SUCCESS);

					} catch (Exception e) {
						errorMessage = BroadBandCommonUtils.concatStringUsingStringBuffer(errorMessage,
								BroadBandTestConstants.COLON_AND_SPACE, e.getMessage());
					}
					if (result) {
						LOGGER.info(
								"STEP 15: ACTUAL : Tr69 set request success and parameter value is Test_Telemetry_Tag");
					} else {
						LOGGER.error("STEP 15: ACTUAL : " + errorMessage);
					}

					LOGGER.info("**********************************************************************************");
					tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

					step = "s16";
					result = false;
					LOGGER.info("**********************************************************************************");
					LOGGER.info(
							"STEP 16: DESCRIPTION : Set value of UniqueTelemetryId TimingInterval parameter using tr69");
					LOGGER.info("STEP 16: ACTION : Execute tr69 command to set value of "
							+ "Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.UniqueTelemetryId.TimingInterval to 10");
					LOGGER.info("STEP 16: EXPECTED : Tr69 set request success and parameter value is 10");
					LOGGER.info("**********************************************************************************");
					try {
						errorMessage = "Unable to set value of UniqueTelemetryId TimingInterval parameter using tr69";

						Parameter setParam = new Parameter();

						setParam.setDataType(TR69ParamDataType.INT.get());
						setParam.setParamName(BroadBandWebPaConstants.WEBPA_PARAM_UNIQUE_TELEMETRY_TIMINGINTERVAL);
						setParam.setParamValue(BroadBandTestConstants.STRING_VALUE_TEN);
						List<Parameter> parameters = new ArrayList<Parameter>();
						parameters.add(setParam);
						response = tapEnv.setTR69ParameterValues(device, parameters);
						result = CommonMethods.isNotNull(response)
								&& response.equalsIgnoreCase(AutomaticsConstants.SUCCESS);

					} catch (Exception e) {
						errorMessage = e.getMessage();
					}
					if (result) {
						LOGGER.info("STEP 16: ACTUAL : Tr69 set request success and parameter value is 10");
					} else {
						LOGGER.error("STEP 16: ACTUAL : " + errorMessage);
					}
					LOGGER.info("**********************************************************************************");
					tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

					step = "s17";
					result = false;
					LOGGER.info("**********************************************************************************");
					LOGGER.info("STEP 17: DESCRIPTION : Set value of UniqueTelemetryId Enable parameter using tr69");
					LOGGER.info("STEP 17: ACTION : Execute tr69 command to set value of "
							+ "Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.UniqueTelemetryId.Enable to true");
					LOGGER.info("STEP 17: EXPECTED : Tr69 set request success and parameter value is true");
					LOGGER.info("**********************************************************************************");
					try {
						errorMessage = "Unable to set value of UniqueTelemetryId Enable parameter using tr69";

						Parameter setParam = new Parameter();

						setParam.setDataType(TR69ParamDataType.BOOLEAN.get());
						setParam.setParamName(BroadBandWebPaConstants.WEBPA_PARAM_UNIQUE_TELEMETRY_ENABLE);
						setParam.setParamValue(BroadBandTestConstants.TRUE);
						List<Parameter> parameters = new ArrayList<Parameter>();
						parameters.add(setParam);
						response = tapEnv.setTR69ParameterValues(device, parameters);
						result = CommonMethods.isNotNull(response)
								&& response.equalsIgnoreCase(AutomaticsConstants.SUCCESS);

					} catch (Exception e) {
						errorMessage = e.getMessage();
					}
					if (result) {
						LOGGER.info("STEP 17: ACTUAL : Tr69 set request success and parameter value is true");
					} else {
						LOGGER.error("STEP 17: ACTUAL : " + errorMessage);
					}

					LOGGER.info("**********************************************************************************");
					tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

					step = "s18";
					result = false;
					LOGGER.info("**********************************************************************************");
					LOGGER.info("STEP 18: DESCRIPTION : Verify configured tag string is printed in Arm/Consolelog");
					LOGGER.info(
							"STEP 18: ACTION : Execute command: grep \"Test_Telemetry_Tag\" /rdklogs/logs/Consolelog.txt.0"
									+ " (ArmConsolelog.txt.0 for Atom devices)");
					LOGGER.info("STEP 18: EXPECTED : Configured log message is printed in Arm/Consolelog");
					LOGGER.info("**********************************************************************************");
					errorMessage = "Unable to find configured tag string printed in Arm/Consolelog";
					startTime = System.currentTimeMillis();
					do {
						response = BroadBandCommonUtils.searchLogsInArmConsoleOrConsole(device, tapEnv,
								BroadBandTestConstants.TELEMETRY_MARKER_TEST_TELEMETRY_TAG);
						result = CommonMethods.isNotNull(response) && CommonMethods.patternMatcher(response,
								BroadBandTestConstants.TELEMETRY_MARKER_TEST_TELEMETRY_TAG);
					} while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.TEN_MINUTE_IN_MILLIS
							&& !result && BroadBandCommonUtils.hasWaitForDuration(tapEnv,
									BroadBandTestConstants.ONE_MINUTE_IN_MILLIS));
					if (result) {
						LOGGER.info("STEP 18: ACTUAL : Configured log message is printed in Arm/Consolelog");
					} else {
						LOGGER.error("STEP 18: ACTUAL : " + errorMessage);
					}
					LOGGER.info("**********************************************************************************");
					tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

					step = "s19";
					result = false;
					LOGGER.info("**********************************************************************************");
					LOGGER.info("STEP 19: DESCRIPTION : Verify telemetry logging for configured tag in dcmscript.log"
							+ " with timestamp later than timestamp obtained in step 14");
					LOGGER.info(
							"STEP 19: ACTION : Execute command: grep \"Test_Telemetry_Tag\" /rdklogs/logs/dcmscript.log");
					LOGGER.info("STEP 19: EXPECTED : Telemetry log message is present for configured tag string"
							+ " with timestamp later than timestamp in step 14");
					LOGGER.info("**********************************************************************************");
					errorMessage = "Unable to find telemetry logging in dcmscript log file for configured telemetry marker";
					startTime = System.currentTimeMillis();
					if (!isTelemetryEnabled) {
						do {
							result = BroadBandCommonUtils.verifyConsoleLogByPassingDateFormat(tapEnv, device,
									BroadBandTestConstants.TELEMETRY_MARKER_TEST_TELEMETRY_TAG,
									BroadBandTestConstants.DCMSCRIPT_LOG_FILE, deviceTimeStamp,
									BroadBandTestConstants.PATTERN_MATCHER_TIMESTAMP_SPEEDTEST_LOG_MESSAGE,
									BroadBandTestConstants.TIMESTAMP_FORMAT_SPEEDTEST_LOG_MESSAGE);
						} while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS
								&& !result && BroadBandCommonUtils.hasWaitForDuration(tapEnv,
										BroadBandTestConstants.ONE_MINUTE_IN_MILLIS));
						if (result) {
							LOGGER.info(
									"STEP 19: ACTUAL : Telemetry log message is present for configured tag string with timestamp\"\r\n"
											+ "					+ \" later than timestamp in step 14\"");
						} else {
							LOGGER.error("STEP 19: ACTUAL : " + errorMessage);
						}

						LOGGER.info(
								"**********************************************************************************");
						tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);
					} else {

						errorMessage = "Test step not applicable when for telemtry 2.0 ";
						LOGGER.info(
								"**********************************************************************************");

						tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_APPLICABLE,
								errorMessage, false);

					}

				}
			}

			step = "s20";
			result = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 20: DESCRIPTION : Reset values of UniqueTelemetryId parameters to default");
			LOGGER.info("STEP 20: ACTION : Reset values of UniqueTelemetryId parameters using webpa"
					+ " or dmcli to false, 0 and null");
			LOGGER.info("STEP 20: EXPECTED : Parameter values reset successfully");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to reset values of UniqueTelemetryId parameters to default successfully";
			result = BroadBandTelemetryUtils.resetDefaultConfigurableTelemetryParameters(device, tapEnv);
			if (result) {
				LOGGER.info("STEP 20: ACTUAL : Parameter values reset successfully");
			} else {
				LOGGER.error("STEP 20: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

			step = "s21";
			result = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 21: DESCRIPTION : Clear Arm/Consolelog, dcmscript.log and obtain device timestamp");
			LOGGER.info("STEP 21: ACTION : Execute echo clear commands for Arm/Consolelog, dcmscript.log"
					+ " and date command for device timestamp");
			LOGGER.info("STEP 21: EXPECTED : Cleared log files and obtained timestamp successfully");
			LOGGER.info("**********************************************************************************");
			errorMessage = BroadBandCommonUtils.concatStringUsingStringBuffer("Unable to clear ",
					CommonMethods.isAtomSyncAvailable(device, tapEnv) ? BroadBandCommandConstants.FILE_ARMCONSOLELOG
							: BroadBandCommandConstants.FILE_CONSOLELOG,
					" file successfully");
			if (CommonUtils.clearLogFile(tapEnv, device,
					CommonMethods.isAtomSyncAvailable(device, tapEnv) ? BroadBandCommandConstants.FILE_ARMCONSOLELOG
							: BroadBandCommandConstants.FILE_CONSOLELOG)) {
				errorMessage = "Unable to clear dcmscript.log file successfully";
				if (CommonUtils.clearLogFile(tapEnv, device, BroadBandCommandConstants.FILE_DCMRFC_LOG)) {
					errorMessage = "Unable to retrieve timestamp from the device";
					deviceTimeStamp = BroadBandCommonUtils.getCurrentTimeStampOnDevice(tapEnv, device);
					result = CommonMethods.isNotNull(deviceTimeStamp);
				}
			}
			if (result) {
				LOGGER.info("STEP 21: ACTUAL : Cleared log files and obtained timestamp successfully");
			} else {
				LOGGER.error("STEP 21: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

			step = "s22";
			result = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 22: DESCRIPTION : Set value of UniqueTelemetryId TagString parameter using webpa");
			LOGGER.info("STEP 22: ACTION : Execute webpa command to set value of "
					+ "Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.UniqueTelemetryId.TagString to Test_Telemetry_Tag");
			LOGGER.info("STEP 22: EXPECTED : WebPA set request success and parameter value is Test_Telemetry_Tag");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to set value of UniqueTelemetryId TagString parameter using webpa";
			result = BroadBandWebPaUtils.verifyWebPaValueAfterDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_UNIQUE_TELEMETRY_TAGSTRING, BroadBandTestConstants.CONSTANT_0,
					BroadBandTestConstants.TELEMETRY_MARKER_TEST_TELEMETRY_TAG, BroadBandTestConstants.CONSTANT_0);
			if (result) {
				LOGGER.info("STEP 22: ACTUAL : WebPA set request success and parameter value is Test_Telemetry_Tag");
			} else {
				LOGGER.error("STEP 22: ACTUAL : " + errorMessage);
			}

			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

			step = "s23";
			result = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 23: DESCRIPTION : Set value of UniqueTelemetryId TimingInterval parameter using webpa");
			LOGGER.info("STEP 23: ACTION : Execute webpa command to set value of "
					+ "Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.UniqueTelemetryId.TimingInterval to 5");
			LOGGER.info("STEP 23: EXPECTED : WebPA set request success and parameter value is 5");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to set value of UniqueTelemetryId TimingInterval parameter using webpa";
			result = BroadBandWebPaUtils.verifyWebPaValueAfterDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_UNIQUE_TELEMETRY_TIMINGINTERVAL,
					BroadBandTestConstants.CONSTANT_1, BroadBandTestConstants.STRING_VALUE_FIVE,
					BroadBandTestConstants.CONSTANT_0);
			if (result) {
				LOGGER.info("STEP 23: ACTUAL : WebPA set request success and parameter value is 5");
			} else {
				LOGGER.error("STEP 23: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

			step = "s24";
			result = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 24: DESCRIPTION : Set value of UniqueTelemetryId Enable parameter using webpa");
			LOGGER.info("STEP 24: ACTION : Execute webpa command to set value of "
					+ "Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.UniqueTelemetryId.Enable to true");
			LOGGER.info("STEP 24: EXPECTED : WebPA set request success and parameter value is true");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to set value of UniqueTelemetryId Enable parameter using webpa";
			result = BroadBandWebPaUtils.verifyWebPaValueAfterDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_UNIQUE_TELEMETRY_ENABLE, BroadBandTestConstants.CONSTANT_3,
					BroadBandTestConstants.TRUE, BroadBandTestConstants.CONSTANT_0);
			if (result) {
				LOGGER.info("STEP 24: ACTUAL : WebPA set request success and parameter value is true");
			} else {
				LOGGER.error("STEP 24: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

			step = "s25";
			result = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 25: DESCRIPTION : Verify configured tag string is printed in PAMlog");
			LOGGER.info("STEP 25: ACTION : Execute command: grep \"Test_Telemetry_Tag\" /rdklogs/logs/PAMlog.txt.0");
			LOGGER.info("STEP 25: EXPECTED : Configured log message is printed in PAMlog");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to find configured tag string printed in PAMlog";
			startTime = System.currentTimeMillis();
			do {
				response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
						BroadBandTestConstants.TELEMETRY_MARKER_TEST_TELEMETRY_TAG,
						BroadBandTestConstants.COMMAND_NTP_LOG_FILE, BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
						BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
				result = CommonMethods.isNotNull(response) && CommonMethods.patternMatcher(response,
						BroadBandTestConstants.TELEMETRY_MARKER_TEST_TELEMETRY_TAG);
			} while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS && !result
					&& BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS));
			if (result) {
				LOGGER.info("STEP 25: ACTUAL : Configured log message is printed in PAMlog");
			} else {
				LOGGER.error("STEP 25: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

			step = "s26";
			result = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 26: DESCRIPTION : Clear log file and verify configured tag string is printed again"
					+ " after configured timing interval period");
			LOGGER.info("STEP 26: ACTION : Execute echo clear PAMlog file and execute grep"
					+ " Test_Telemetry_Tag command");
			LOGGER.info("STEP 26: EXPECTED : Configured log message is printed in PAMlog"
					+ " after configured timing interval");
			LOGGER.info("**********************************************************************************");
			errorMessage = BroadBandCommonUtils.concatStringUsingStringBuffer("Unable to clear ",
					BroadBandTestConstants.COMMAND_NTP_LOG_FILE, " file successfully");

			errorMessage = "Unable to find configured tag string printed in PAMlog after configured"
					+ " timing interval (2 minutes)";
			startTime = System.currentTimeMillis();
			do {
				response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
						BroadBandTestConstants.TELEMETRY_MARKER_TEST_TELEMETRY_TAG,
						BroadBandTestConstants.COMMAND_NTP_LOG_FILE, BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS,
						BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
				result = CommonMethods.isNotNull(response) && CommonMethods.patternMatcher(response,
						BroadBandTestConstants.TELEMETRY_MARKER_TEST_TELEMETRY_TAG);
			} while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS && !result
					&& BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS));

			if (result) {
				LOGGER.info(
						"STEP 26: ACTUAL : Configured log message is printed in PAMlog after configured timing interval");
			} else {
				LOGGER.error("STEP 26: ACTUAL : " + errorMessage);
			}

			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

			step = "s27";
			result = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 27: DESCRIPTION : Verify telemetry logging for configured tag in dcmscript.log"
					+ " with timestamp later than timestamp obtained in step 21");
			LOGGER.info("STEP 27: ACTION : Execute command: grep \"Test_Telemetry_Tag\" /rdklogs/logs/dcmscript.log");
			LOGGER.info("STEP 27: EXPECTED : Telemetry log message is present for configured tag string"
					+ " with timestamp later than timestamp in step 21");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to find telemetry logging in dcmscript log file for configured telemetry marker";
			startTime = System.currentTimeMillis();
			if (!isTelemetryEnabled) {
				do {
					result = BroadBandCommonUtils.verifyConsoleLogByPassingDateFormat(tapEnv, device,
							BroadBandTestConstants.TELEMETRY_MARKER_TEST_TELEMETRY_TAG,
							BroadBandTestConstants.DCMSCRIPT_LOG_FILE, deviceTimeStamp,
							BroadBandTestConstants.PATTERN_MATCHER_TIMESTAMP_SPEEDTEST_LOG_MESSAGE,
							BroadBandTestConstants.TIMESTAMP_FORMAT_SPEEDTEST_LOG_MESSAGE);
				} while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS
						&& !result && BroadBandCommonUtils.hasWaitForDuration(tapEnv,
								BroadBandTestConstants.ONE_MINUTE_IN_MILLIS));
				if (result) {
					LOGGER.info(
							"STEP 27: ACTUAL : Telemetry log message is present for configured tag string with timestamp later than timestamp in step 21");
				} else {
					LOGGER.error("STEP 27: ACTUAL : " + errorMessage);
				}

				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);
			} else {

				errorMessage = "Test step not applicable for telemtry 2.0 ";
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);

			}

			step = "s28";
			result = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 28: DESCRIPTION : Set value of UniqueTelemetryId TagString parameter with value"
					+ " greater than 255 characters");
			LOGGER.info("STEP 28: ACTION : Execute webpa command to set value of "
					+ "Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.UniqueTelemetryId.TagString to very large string");
			LOGGER.info("STEP 28: EXPECTED : WebPA set request failed to set value greater than 255 characters");
			LOGGER.info("**********************************************************************************");
			errorMessage = "UniqueTelemetryId TagString parameter is supporting set of value greater than 255 characters";
			result = !BroadBandWebPaUtils.verifyWebPaValueAfterDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_UNIQUE_TELEMETRY_TAGSTRING, BroadBandTestConstants.CONSTANT_0,
					BroadBandTestConstants.STRING_WITH_260_CHAR_LENGTH, BroadBandTestConstants.CONSTANT_0);
			if (result) {
				LOGGER.info("STEP 28: ACTUAL : WebPA set request failed to set value greater than 255 characters");
			} else {
				LOGGER.error("STEP 28: ACTUAL : " + errorMessage);
			}

			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

			step = "s29";
			result = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 29: DESCRIPTION : Verify invalid string is not logged in Arm/Consolelog");
			LOGGER.info("STEP 29: ACTION : Execute grep command to verify large string not present in Arm/Consolelog");
			LOGGER.info("STEP 29: EXPECTED : Configured invalid string is not printed in Arm/Consolelog");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Invalid string (length > 255 char)  is getting logged in Arm/Consolelog";
			startTime = System.currentTimeMillis();
			do {
				result = CommonMethods.isNull(BroadBandCommonUtils.searchLogsInArmConsoleOrConsole(device, tapEnv,
						BroadBandTestConstants.STRING_WITH_260_CHAR_LENGTH));
			} while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS && result
					&& BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS));
			if (result) {
				LOGGER.info("STEP 29: ACTUAL : Configured invalid string is not printed in Arm/Consolelog");
			} else {
				LOGGER.error("STEP 29: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

			step = "s30";
			result = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 30: DESCRIPTION : Reset values of UniqueTelemetryId parameters to default");
			LOGGER.info("STEP 30: ACTION : Reset values of UniqueTelemetryId parameters using webpa"
					+ " or dmcli to false, 0 and null");
			LOGGER.info("STEP 30: EXPECTED : Parameter values reset successfully");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to reset values of UniqueTelemetryId parameters to default successfully";
			result = BroadBandTelemetryUtils.resetDefaultConfigurableTelemetryParameters(device, tapEnv);
			if (result) {
				LOGGER.info("STEP 30: ACTUAL : Parameter values reset successfully");
			} else {
				LOGGER.error("STEP 30: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

		} catch (Exception exception) {
			errorMessage = BroadBandCommonUtils.concatStringUsingStringBuffer(errorMessage,
					BroadBandTestConstants.COLON_AND_SPACE, exception.getMessage());
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, step, result, errorMessage, true);

		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS");
			LOGGER.info("POST-CONDITION : DESCRIPTION : Remove nvram override and reboot");
			LOGGER.info("POST-CONDITION : ACTION : Remove DCMSettings.conf files in /nvram or /tmp and"
					+ " dcm.properties in /nvram and execute reboot command");
			LOGGER.info("POST-CONDITION : EXPECTED : Post conditions executed successfully");

			if (CommonUtils.isFileExists(device, tapEnv, BroadBandTestConstants.DCM_PROPERTIES_FILE_NVRAM_FOLDER)) {
				result = BroadBandTelemetryUtils.clearTelemetryConfiguration(tapEnv, device)
						&& BroadBandRfcFeatureControlUtils.removeNvramOverrideForRfc(device, tapEnv);
			} else {
				LOGGER.info("File /nvram/dcm.properties not present, skipping post condition");
				result = true;
			}
			BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
			LOGGER.info("POST-CONDITION : ACTUAL : "
					+ (result ? "Post condition executed successfully" : "Post condition failed"));
			LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + result);
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-TELEMETRY-1003");
		LOGGER.info("#######################################################################################");
	}
	
    /**
     * Method to verifyCronTabProcess
     * 
     * @param device
     *            {@link Dut}
     * @param testId
     *            Test case ID
     * @param stepNumber
     *            Step Number
     * @return boolean postExecution status
     * 
     * @Refactor Sruthi Santhosh
     */
    public static boolean verifyCronTabProcess(Dut device, String testCaseId, int stepNumber) throws TestException {

    // Variable Declaration begins
	String errorMessage = null;
	boolean status = false;
	String stepNum = null;
	String response = null;
	boolean postExecution = false;
	long boxUpTimeInSeconds = 0;
	boolean isSTBAccessible = false;
	// Variable Declaration ends
	try {

	    /**
	     * Step 10 : Verify cron job is scheduled
	     */
	    stepNumber++;
	    stepNum = "s" + stepNumber;
	    errorMessage = "Unable to retrieve crontab processes either from crontab -l/crotab -l -c /var/spool/cron/crontabs";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify  cron tab processes is running.");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute command:crontab -l -c /var/spool/cron/crontabs and command:crontab -l");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED : Response should contain cron tab processes and it should be same for both the commands executed.");
	    LOGGER.info("**********************************************************************************");
	    String cronTabResponse = null;
	    errorMessage = "Failed to enable periodic firmware check";
	    // to enable periodic firmware check is enabled to get firmware download process in crontab.
	    try {
		status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_PERIODIC_FW_CHECK_ENABLE,
			BroadBandTestConstants.BOOLEAN_VALUE_TRUE.toString(),
			BroadBandTestConstants.ONE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		if (!status) {
		    status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
			    BroadBandWebPaConstants.WEBPA_PARAM_PERIODIC_FW_CHECK_ENABLE,
			    BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE,
			    BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS,
			    BroadBandTestConstants.FIFTY_SECONDS_IN_MILLIS);
		    postExecution = status;
		    // waiting for twenty seconds for rabid process disable changes to get reflected
		    tapEnv.waitTill(BroadBandTestConstants.THREE_MINUTE_IN_MILLIS);
		}
	    } catch (Exception exception) {
		LOGGER.error(errorMessage + " : " + exception.getMessage());
	    }
	    // while condition is to wait till the firmware download process is running in crontab processes
	    errorMessage = "Failed to get the crontab processes either in crontab -l or crontab -l -c /var/spool/cron/crontabs commands.";
	    tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	    do {
		isSTBAccessible = CommonMethods.isSTBAccessible(device);
		if (!isSTBAccessible) {
		    LOGGER.error("Device is not accessible while checking for cron tab process is up and running");
		}
		boxUpTimeInSeconds = CommonUtils.getBoxUptimeInSeconds(device, tapEnv);
		cronTabResponse = tapEnv.executeCommandUsingSsh(device,
			BroadBandCommandConstants.CRONTAB_EXECUTE_COMMAND);
		status = cronTabResponse.contains(BroadBandCommandConstants.FIRMWARE_SCRIPT_GENERIC_NAME);
	    } while (!status && (boxUpTimeInSeconds * 1000 < BroadBandTestConstants.SIXTEEN_MINUTES_IN_MILLIS)
		    && isSTBAccessible && BroadBandCommonUtils.hasWaitForDuration(tapEnv,
			    BroadBandTestConstants.FIFTEEN_SECONDS_IN_MILLIS));
	    // while condition status check is not required as the below command is to retrieve crontab processes using
	    // absolute path.
	    try {
		response = tapEnv.executeCommandUsingSsh(device,
			BroadBandCommandConstants.CRONTAB_EXECUTE_ABSOLUTEPATH);
		status = CommonMethods.isNotNull(cronTabResponse) && CommonMethods.isNotNull(response)
			&& BroadBandCommonUtils.compareValues(BroadBandTestConstants.CONSTANT_TXT_COMPARISON,
				cronTabResponse, response);
	    } catch (Exception exception) {
		status = false;
		LOGGER.error(errorMessage + " : " + exception.getMessage());
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : Verified cron tab process obtained from absolute path");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	}
	return postExecution;
    }
}
