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
import com.automatics.rdkb.constants.BroadBandTelemetryConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandPreConditionUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.dmcli.DmcliUtils;
import com.automatics.rdkb.utils.telemetry.BroadBandTelemetryUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

public class BroadBandTelemetryBasicTests extends AutomaticsTestBase {

    /**
     * Verify the CPU and Memory usage individually per process
     * <ol>
     * <li>verify the log file /rdklogs/logs/CPUInfo.txt.0 to find the CPU and Memory usage for each individual
     * processes that are running</li>
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
     * <li>Copy dcm.properties to /nvram folder and change the DCM_LOG_SERVER_URL</li>
     * <li>Reboot the device and wait for IP acquisition</li>
     * <li>Validate modified url in dcmscript.log file</li>
     * <li>Verify whether telemetry is scheduled to 5 min in atom console</li>
     * <li>Verify whether DCMresponse.txt file is available in the device under /nvram</li>
     * <li>Verify whether cpu and memory telemetry data is available in the /rdklogs/logs/dcmscript.log</li>
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
     * This method is to verify the telemetry marker for cpu and memory usage from the log file
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
     * <li>Verify that the old STBRTL url is present in /rdklogs/logs/dcmscript.log</li>
     * <li>Set value of parameter Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.TelemetryEndpoint.URL to new STBRTL
     * url</li>
     * <li>reboot device and verify that Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.TelemetryEndpoint.URL is
     * enabled</li>
     * <li>Verify that the old STBRTL url is still present and /rdklogs/logs/dcmscript.log and should not be updated to
     * new url</li>
     * <li>set value of parameter Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.TelemetryEndpoint.Enable to true</li>
     * <li>reboot and Verify that the telemetry endpoint url should be changed to new STBRTL url in
     * /opt/logs/dcmscript.log</li>
     * <li>Verify that the telemetry endpoint url should be changed to new STBRTL url in
     * /rdklogs/logs/dcmscript.log</li>
     * <li>Endpoint for telemetry should include a single 30 second timeout and is logged in
     * /rdklogs/logs/dcmscript.log</li>
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
     * <li>Verify value of parameter uploaded in telemetry is same as actual parameter value</li>
     * <li>Verify log message for telemetry data source not found for invalid parameter</li>
     * <li>Verify invalid parameter telemetry marker does not appear in dcmscript.log</li>
     * <li>Wait for 'TEST_MULTI_INSTANCE' marker to appear</li>
     * <li>Verify log message for skipping telemetry object for invalid format parameter</li>
     * <li>Verify invalid Multi instance parameter telemetry marker does not appear in dcmscript.log</li>
     * <li>Disable Telemetry message bus source parameter</li>
     * <li>Check disabled log message in dcmProcessing.log</li>
     * <li>Enable Telemetry message bus source parameter</li>
     * </ol>
     * 
     * @param device
     *            {@link Dut}
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

}
