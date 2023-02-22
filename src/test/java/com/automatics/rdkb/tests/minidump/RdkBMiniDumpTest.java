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

package com.automatics.rdkb.tests.minidump;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.AutomaticsConstants;
import com.automatics.constants.CrashConstants;
import com.automatics.constants.DataProviderConstants;
import com.automatics.core.DeviceProcess;
import com.automatics.core.SupportedModelHandler;
import com.automatics.dataobjects.StbDetailsDO.StbDetails;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.enums.ProcessRestartOption;
import com.automatics.enums.StbProcess;
import com.automatics.exceptions.TestException;
import com.automatics.providers.crashanalysis.CrashDetails;
import com.automatics.providers.crashanalysis.CrashType;
import com.automatics.providers.crashanalysis.SettopCrashUtils;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandPropertyKeyConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.WebPaParamConstants.WebPaDataTypes;
import com.automatics.rdkb.tests.base.BroadBandMiniDumpBaseTest;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandPostConditionUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.ResultValues;
import com.automatics.rdkb.utils.tr69.BroadBandTr69Utils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.utils.AutomaticsPropertyUtility;
import com.automatics.utils.CommonMethods;

public class RdkBMiniDumpTest extends BroadBandMiniDumpBaseTest {

	/** Default interface to retrieve estb mac */
	private static final String DEFAULT_INTERFACE_NAME_TO_RETRIEVE_ESTB_MAC_ADDRESS = "erouter0";

	/**
	 * This method is written to add mini dumps to S3 amazon locations' .This method
	 * tests the generation of minidump when Ccsp process is killed and verifies the
	 * minidump upload to: 1.Amazon S3 server with correct S3 Amazon Signing Url 2.
	 * Crash portal as fail over mechanism with incorrect S3 Amazon Signing Url
	 * 
	 * <ol>
	 * <li>Step 1: Obtain Pid of CcspPandMSsp process.</li>
	 * <li>Step 2: Create a file /nvram/coredump.properties and override the
	 * S3_AMAZON_SIGNING_URL</li>
	 * <li>Step 3: Kill CcspPandMSsp process and verify it has started again.</li>
	 * <li>Step 4: Poll and check core_log.txt for upload failed and retry log
	 * messages</li>
	 * <li>Step 5: Poll and check for fail over mechanism log message is not present
	 * in core_log.txt. If Fail over mechanism fails, need to check for that log
	 * message is not present</li>
	 * <li>Step 6: Verify minidump is not uploaded to new Failover servers</li>
	 * <li>Step 7: Check if minidump folder has been cleaned up and the log messages
	 * present.</li>
	 * <li>Step 8: Remove /nvram/coredump.properties file</li>
	 * <li>Step 9: Obtain Pid of CcspPandMSsp process.</li>
	 * <li>Step 10: Kill CcspPandMSsp process and verify it has started again.</li>
	 * <li>Step 11: Poll and check for S3 Amazon upload success log message in
	 * core_log.txt</li>
	 * <li>Step 12: Check if minidump folder has been cleaned up and the log
	 * messages present.</li>
	 * </ol>
	 * 
	 * @author Ashwin Sankarasubramanian
	 * @refactor Said Hisham
	 * 
	 * @param device device to be used for execution
	 * 
	 */
	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
			BroadBandTestGroup.NEW_FEATURE, BroadBandTestGroup.SYSTEM })
	@TestDetails(testUID = "TC-RDKB-SYSTEM-1026")
	public void testVerifyAmazonS3Upload(Dut device) {

		// stores the test result
		boolean result = false;
		boolean status = false;
		// stores the test case id
		String testId = "TC-RDKB-SYSTEM-026";
		// stores the error message
		String errorMessage = null;
		// stores the command response
		String response = null;
		// stores the test step number
		String step = "s1";
		// stores the Result values response
		ResultValues resValues = new ResultValues();
		// stores the message list to be passed as parameter
		List<String> messageList = new ArrayList<String>();
		// stores the start time for poll duration
		long startTime = 0;

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-SYSTEM-1026");
		LOGGER.info("TEST DESCRIPTION: verify minidump upload to S3/crash portal");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Obtain Pid of CcspPandMSsp process.");
		LOGGER.info("2. Create a file /nvram/coredump.properties and override the S3_AMAZON_SIGNING_URL");
		LOGGER.info("3. Kill CcspPandMSsp process and verify it has started again.");
		LOGGER.info("4. Poll and check core_log.txt for upload failed and retry log messages ");
		LOGGER.info(
				"5. Poll and check for fail over mechanism log message not present in core_log.txt. If Fail over mechanism fails,need to check for that log message is not present");
		LOGGER.info("6. Verify minidump is not uploaded to new Failover servers.");
		LOGGER.info("7. Check if minidump folder has been cleaned up and the log messages present");
		LOGGER.info("8. Remove /nvram/coredump.properties file");
		LOGGER.info("9. Obtain Pid of CcspPandMSsp process.");
		LOGGER.info("10. Kill CcspPandMSsp process and verify it has started again.");
		LOGGER.info("11. Poll and check for S3 Amazon upload success log message in core_log.txt");
		LOGGER.info("12. Check if minidump folder has been cleaned up and the log messages present");
		LOGGER.info("#######################################################################################");
		try {

			if (DeviceModeHandler.isRPIDevice(device)) {

				LOGGER.info("removing previously generated dump files");
				response = tapEnv.executeCommandUsingSsh(device, BroadBandTestConstants.CMD_REMOVE_FORCEFULLY
						+ BroadBandTestConstants.STRING_PARTITION_MINIDUMPS + "/" + BroadBandTestConstants.ASTERISK);
				response = tapEnv.executeCommandUsingSsh(device,
						BroadBandCommandConstants.CMD_LS + BroadBandTestConstants.SINGLE_SPACE_CHARACTER
								+ BroadBandTestConstants.STRING_PARTITION_MINIDUMPS);
				if (!response.contains(BroadBandTestConstants.NO_SUCH_FILE_OR_DIRECTORY)) {
					if (CommonMethods.isNull(response)) {
						status = true;
					}
				} else {
					tapEnv.executeCommandUsingSsh(device,
							BroadBandCommandConstants.CMD_MKDIR + BroadBandTestConstants.STRING_PARTITION_MINIDUMPS);
					status = true;
				}
			}

			/*
			 * Step 1: Obtain Pid of CcspPandMSsp process.
			 */
			errorMessage = "Unable to obtain the pid for CcspPandMSsp";
			LOGGER.info("*****************************************************************************************");
			LOGGER.info("STEP 1:DESCRIPTION: Obtain Pid of CcspPandMSsp process.");
			LOGGER.info("STEP 1:ACTION: Execute command:pidof CcspPandMSsp");
			LOGGER.info("STEP 1:EXPECTED - Pid of CcspPandMSsp process is obtained");
			LOGGER.info("*****************************************************************************************");
			response = CommonMethods.getPidOfProcess(device, tapEnv, StbProcess.PANDM.getProcessName());
			result = CommonMethods.isNotNull(response);
			if (result) {
				LOGGER.info("STEP 1: ACTUAL :Pid of CcspPandMSsp process is obtained");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}
			LOGGER.info("*****************************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, step, result, errorMessage, true);

			/*
			 * Step 2: Create a file /nvram/coredump.properties and override the
			 * S3_AMAZON_SIGNING_URL
			 */
			step = "s2";
			errorMessage = "Unable to create file /nvram/coredump.properties with incorrect url";
			result = false;
			if (!DeviceModeHandler.isRPIDevice(device)) {
				LOGGER.info(
						"*****************************************************************************************");
				LOGGER.info(
						"STEP 2:DESCRIPTION:  Create a file /nvram/coredump.properties and override the S3_AMAZON_SIGNING_URL.");
				LOGGER.info(
						"STEP 2:ACTION: Execute command: echo 'S3_AMAZON_SIGNING_URL=http://test' > /nvram/coredump.properties");
				LOGGER.info("STEP 2:EXPECTED - File is created with incorrect S3 Amazon Signing Url");
				LOGGER.info(
						"*****************************************************************************************");
				tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_AMAZON_URL_OVERRIDE);
				response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
						BroadBandTestConstants.PROP_KEY_AMAZON_URL,
						BroadBandCommandConstants.FILE_NVRAM_COREDUMP_PROPERTIES);
				result = CommonMethods.isNotNull(response);
				if (result) {
					LOGGER.info("STEP 2: ACTUAL :File is created with incorrect S3 Amazon Signing Url");
				} else {
					LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
				}
				LOGGER.info(
						"*****************************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, step, result, errorMessage, true);
			} else {
				LOGGER.info("upload repository not configured : skipping teststep...");
				tapEnv.updateExecutionForAllStatus(device, testId, step, ExecutionStatus.NOT_APPLICABLE, errorMessage,
						false);
			}

			/*
			 * Step 3: Kill CcspPandMSsp process and verify it has started again.
			 */
			step = "s3";
			errorMessage = "Unable to kill and verify CcspPandMSsp restarted with new pid";
			result = false;
			LOGGER.info("*****************************************************************************************");
			LOGGER.info("STEP 3:DESCRIPTION: Kill CcspPandMSsp and verify it has started again.");
			LOGGER.info("STEP 3:ACTION: Execute command:kill -11 <pid_of_ccsp_process>.");
			LOGGER.info("STEP 3:EXPECTED - CcspPandMSsp has been killed and started with new pid");
			LOGGER.info("*****************************************************************************************");
			result = CommonMethods.restartProcess(device, tapEnv, ProcessRestartOption.KILL_11,
					StbProcess.PANDM.getProcessName());
			if (result) {
				LOGGER.info("STEP 3: ACTUAL :CcspPandMSsp has been killed and started with new pid");
			} else {
				LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
			}
			LOGGER.info("*****************************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, step, result, errorMessage, true);

			/*
			 * Step 4: Poll and check core_log.txt for upload failed and retry log messages
			 */
			step = "s4";
			result = false;
			if (BroadbandPropertyFileHandler.isServerConfiguredToUploadCrashDetails()) {
				LOGGER.info(
						"*****************************************************************************************");
				LOGGER.info(
						"STEP 4:DESCRIPTION: Poll and check core_log.txt for upload failed and retry log messages.");
				LOGGER.info(
						"STEP 4:ACTION: Execute commands:grep -i 'S3 Amazon Upload Failed' /rdklogs/logs/core_log.txt,grep -i 'Retry' /rdklogs/logs/core_log.txt");
				LOGGER.info("STEP 4:EXPECTED - S3 Upload failed and Retry log messages present");
				LOGGER.info(
						"*****************************************************************************************");
				messageList.clear();
				messageList.add(BroadBandTraceConstants.LOG_MESSAGE_MINIDUMP_AMAZON_UPLOAD_FAILED);
				messageList.add(BroadBandTraceConstants.LOG_MESSAGE_MINIDUMP_AMAZON_UPLOAD_RETRY);
				startTime = System.currentTimeMillis();
				do {
					resValues = BroadBandCommonUtils.checkFileForPatterns(device, tapEnv, messageList,
							CrashConstants.LOG_FILE_FOR_CRASHES_RDKB, true);
					result = resValues.isResult();
					errorMessage = resValues.getMessage();
					if (result) {
						break;
					}
				} while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.THREE_MINUTE_IN_MILLIS
						&& BroadBandCommonUtils.hasWaitForDuration(tapEnv,
								BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
				if (result) {
					LOGGER.info("STEP 4: ACTUAL :S3 Upload failed and Retry log messages present");
				} else {
					LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
				}
				LOGGER.info(
						"*****************************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, step, result, errorMessage, false);

				/*
				 * Step 5: Poll and check for fail over mechanism log message in core_log.txt.
				 * If Fail over mechanism fails, need to check for that log message.
				 */
				step = "s5";
				result = false;
				LOGGER.info(
						"*****************************************************************************************");
				LOGGER.info(
						"STEP 5:DESCRIPTION: Poll and check for fail over mechanism log message in core_log.txt. If Fail over mechanism fails, need to check for that log message is not present");
				LOGGER.info(
						"STEP 5:ACTION: Execute command: to search for logs-Fail Over Mechanism: CURL minidump to crashportal,Fail Over Mechanism for minidump : Failed..! in /rdklogs/logs/core_log.txt ");
				LOGGER.info(
						"STEP 5:EXPECTED - Fail over mechansism log messages should not present in /rdklogs/logs/core_log.txt");
				LOGGER.info(
						"*****************************************************************************************");
				messageList.clear();
				messageList.add(BroadBandTraceConstants.LOG_MESSAGE_MINIDUMP_FAIL_OVER_UPLOAD);
				messageList.add(BroadBandTraceConstants.LOG_MESSAGE_MINIDUMP_CRASH_PORTAL_SUCCESS_UPLOAD);
				startTime = System.currentTimeMillis();
				errorMessage = "Fail over mechanism log messages is present in  /rdklogs/logs/core_log.txt";
				do {
					resValues = BroadBandCommonUtils.checkFileForPatterns(device, tapEnv, messageList,
							CrashConstants.LOG_FILE_FOR_CRASHES_RDKB, true);
					result = !resValues.isResult();
					if (!result) {
						messageList.remove(BroadBandTraceConstants.LOG_MESSAGE_MINIDUMP_CRASH_PORTAL_SUCCESS_UPLOAD);
						messageList.add(BroadBandTraceConstants.LOG_MESSAGE_MINIDUMP_FAIL_OVER_FAILED);
						resValues = BroadBandCommonUtils.checkFileForPatterns(device, tapEnv, messageList,
								CrashConstants.LOG_FILE_FOR_CRASHES_RDKB, true);
						result = !resValues.isResult();
					}
					if (result) {
						break;
					}
				} while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.THREE_MINUTE_IN_MILLIS
						&& BroadBandCommonUtils.hasWaitForDuration(tapEnv,
								BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
				if (result) {
					LOGGER.info(
							"STEP 5: ACTUAL :Fail over mechansism log messages is not present in /rdklogs/logs/core_log.txt");
				} else {
					LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
				}
				LOGGER.info(
						"*****************************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, step, result, errorMessage, false);

				/*
				 * Step 6: Verify minidump is uploaded to new Failover servers by checking for
				 * upload log string in core_log.txt file
				 */
				step = "s6";
				errorMessage = "Upload string log message present";
				result = false;
				LOGGER.info(
						"*****************************************************************************************");
				LOGGER.info("STEP 6:DESCRIPTION: Verify minidump is not uploaded to new Failover servers.");
				LOGGER.info("STEP 6:ACTION: Execute command:grep -i 'Upload string' /rdklogs/logs/core_log.txt.");
				LOGGER.info(
						"STEP 6:EXPECTED - The upload string in core_log should not contain new failover upload url");
				LOGGER.info(
						"*****************************************************************************************");
				startTime = System.currentTimeMillis();
				errorMessage = BroadBandCommonUtils
						.concatStringUsingStringBuffer("Upload string contains new crash failover upload url");
				do {
					response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
							BroadBandTraceConstants.LOG_MESSAGE_UPLOAD_STRING,
							CrashConstants.LOG_FILE_FOR_CRASHES_RDKB);
					result = CommonMethods.isNull(response);
					if (!result) {
						errorMessage = BroadBandCommonUtils.concatStringUsingStringBuffer(
								"Upload string contains new crash failover upload url: ",
								AutomaticsPropertyUtility.getProperty(
										BroadBandPropertyKeyConstants.PROP_KEY_RDKB_CRASH_FAILOVER_UPLOAD_URL));
						result = !CommonMethods.patternMatcher(response, AutomaticsPropertyUtility
								.getProperty(BroadBandPropertyKeyConstants.PROP_KEY_RDKB_CRASH_FAILOVER_UPLOAD_URL));
					}
				} while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.TWO_MINUTE_IN_MILLIS
						&& !result && BroadBandCommonUtils.hasWaitForDuration(tapEnv,
								BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS));
				if (result) {
					LOGGER.info(
							"STEP 6: ACTUAL : The upload string in core_log does not contains new failover upload url");
				} else {
					LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
				}
				LOGGER.info(
						"*****************************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, step, result, errorMessage, false);
			} else {
				LOGGER.info("#######################################################################################");
				LOGGER.info(
						"Steps 4 to 6 are not applicable if server details to upload crash details ate not configured");
				LOGGER.info("#######################################################################################");
				tapEnv.updateExecutionForAllStatus(device, testId, "s4", ExecutionStatus.NOT_APPLICABLE,
						"N/A if server details are not configured", false);
				tapEnv.updateExecutionForAllStatus(device, testId, "s5", ExecutionStatus.NOT_APPLICABLE,
						"N/A if server details are not configured", false);
				tapEnv.updateExecutionForAllStatus(device, testId, "s6", ExecutionStatus.NOT_APPLICABLE,
						"N/A if server details are not configured", false);
			}

			/*
			 * Step 7: Check if minidump folder has been cleaned up and the log messages
			 * present.
			 */
			step = "s7";
			result = false;
			if (!DeviceModeHandler.isRPIDevice(device)) {
				LOGGER.info(
						"*****************************************************************************************");
				LOGGER.info(
						"STEP 7:DESCRIPTION: Check if minidump folder has been cleaned up and the log messages present.");
				LOGGER.info(
						"STEP 7:ACTION: Execute commands:grep -i 'Cleanup minidump directory /minidumps' /rdklogs/logs/core_log.txt.");
				LOGGER.info("STEP 7:EXPECTED - The minidump folder is empty and log messages are present");
				LOGGER.info(
						"*****************************************************************************************");
				messageList.clear();
				messageList.add(BroadBandTraceConstants.LOG_MESSAGE_MINIDUMP_CLEANUP_DIRECTORY);
				messageList.add(BroadBandTraceConstants.LOG_MESSAGE_MINIDUMP_WORKING_DIR_EMPTY);
				startTime = System.currentTimeMillis();
				do {
					resValues = BroadBandCommonUtils.checkFileForPatterns(device, tapEnv, messageList,
							CrashConstants.LOG_FILE_FOR_CRASHES_RDKB, true);
					errorMessage = resValues.getMessage();
					result = resValues.isResult();
					if (result) {
						break;
					}
				} while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.THREE_MINUTE_IN_MILLIS
						&& BroadBandCommonUtils.hasWaitForDuration(tapEnv,
								BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
				if (result) {
					LOGGER.info("STEP 7: ACTUAL : The minidump folder is empty and log messages are present");
				} else {
					LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
				}
				LOGGER.info(
						"*****************************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, step, result, errorMessage, false);
			} else {
				LOGGER.info("Check if minidump file is created in RPi");
				String pattern = "(.*.dmp)";
				errorMessage = "minidump file not created in RPi";
				response = tapEnv.executeCommandUsingSsh(device,
						BroadBandCommandConstants.CMD_LS + BroadBandTestConstants.STRING_PARTITION_MINIDUMPS);
				LOGGER.info("response from minidumps folder :" + response);
				result = CommonMethods.patternMatcher(response, pattern);
				if (result) {
					LOGGER.info("STEP 7: ACTUAL : The minidump is created in RPi");
				} else {
					LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
				}
				LOGGER.info(
						"*****************************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, step, result, errorMessage, false);
			}

			/*
			 * Step 8: Remove /nvram/coredump.properties file
			 */
			step = "s8";
			errorMessage = "File was not removed successfully";
			result = false;
			LOGGER.info("*****************************************************************************************");
			LOGGER.info("STEP 8:DESCRIPTION: Remove /nvram/coredump.properties file.");
			LOGGER.info("STEP 8:ACTION: Execute command:rm /nvram/coredump.properties.");
			LOGGER.info("STEP 8:EXPECTED - File /nvram/coredump.properties does not exist");
			LOGGER.info("*****************************************************************************************");

			result = CommonUtils.removeFileandVerifyFileRemoval(tapEnv, device,
					BroadBandCommandConstants.FILE_NVRAM_COREDUMP_PROPERTIES);
			if (result) {
				LOGGER.info("STEP 8: ACTUAL : File /nvram/coredump.properties does not exist");
			} else {
				LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
			}
			LOGGER.info("*****************************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, step, result, errorMessage, true);

			/*
			 * Step 9: Obtain Pid of CcspPandMSsp process.
			 */
			step = "s9";
			errorMessage = "Unable to obtain the pid for CcspPandMSsp";
			result = false;
			LOGGER.info("*****************************************************************************************");
			LOGGER.info("STEP 9:DESCRIPTION: Obtain Pid of CcspPandMSsp process.");
			LOGGER.info("STEP 9:ACTION: Execute command:pidof CcspPandMSsp,echo > /rdklogs/logs/core_log.txt.");
			LOGGER.info("STEP 9:EXPECTED - Pid of CcspPandMSsp process is obtained");
			LOGGER.info("*****************************************************************************************");

			// Clearing core_log file to avoid false pass since we're checking
			// same messages again
			LOGGER.info("Clearing core_log file:");
			if (!DeviceModeHandler.isRPIDevice(device)) {
				CommonUtils.clearLogFile(tapEnv, device, CrashConstants.LOG_FILE_FOR_CRASHES_RDKB);
			} else {
				LOGGER.info("removing previously generated dump files");
				response = tapEnv.executeCommandUsingSsh(device, BroadBandTestConstants.CMD_REMOVE_FORCEFULLY
						+ BroadBandTestConstants.STRING_PARTITION_MINIDUMPS + "/" + BroadBandTestConstants.ASTERISK);
				response = tapEnv.executeCommandUsingSsh(device,
						BroadBandCommandConstants.CMD_LS + BroadBandTestConstants.SINGLE_SPACE_CHARACTER
								+ BroadBandTestConstants.STRING_PARTITION_MINIDUMPS);
				if (!response.contains(BroadBandTestConstants.NO_SUCH_FILE_OR_DIRECTORY)) {
					if (CommonMethods.isNull(response)) {
						status = true;
					}
				} else {
					tapEnv.executeCommandUsingSsh(device,
							BroadBandCommandConstants.CMD_MKDIR + BroadBandTestConstants.STRING_PARTITION_MINIDUMPS);
					status = true;
				}
			}

			response = CommonMethods.getPidOfProcess(device, tapEnv, StbProcess.PANDM.getProcessName());
			result = CommonMethods.isNotNull(response);
			// Restarting PAM process for devices without auto restart
			if (!result) {
				LOGGER.info("Restarting PandM process:");
				tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_START_PANDM_PROCESS);
				response = CommonMethods.getPidOfProcess(device, tapEnv, StbProcess.PANDM.getProcessName());
				result = CommonMethods.isNotNull(response);
			}
			if (result) {
				LOGGER.info("STEP 9: ACTUAL : Pid of CcspPandMSsp process is obtained");
			} else {
				LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
			}
			LOGGER.info("*****************************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, step, result, errorMessage, true);

			/*
			 * Step 10: Kill CcspPandMSsp process and verify it has started again.
			 */
			step = "s10";
			errorMessage = "Unable to kill and verify CcspPandMSsp restarted with new pid";
			result = false;
			LOGGER.info("*****************************************************************************************");
			LOGGER.info("STEP 10:DESCRIPTION: Kill CcspPandMSsp process and verify it has started again.");
			LOGGER.info("STEP 10:ACTION: Execute command:kill -11 <pid_of_ccsp_process>");
			LOGGER.info("STEP 10:EXPECTED - CcspPandMSsp has been killed and started with new pid");
			LOGGER.info("*****************************************************************************************");
			result = CommonMethods.restartProcess(device, tapEnv, ProcessRestartOption.KILL_11,
					StbProcess.PANDM.getProcessName());
			if (result) {
				LOGGER.info("STEP 10: ACTUAL : CcspPandMSsp has been killed and started with new pid");
			} else {
				LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
			}
			LOGGER.info("*****************************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, step, result, errorMessage, true);

			/*
			 * Step 11: Poll and check for S3 Amazon upload success log message in
			 * core_log.txt
			 */
			step = "s11";
			errorMessage = "Unable to find S3 Amazon upload success log message";
			result = false;
			if (BroadbandPropertyFileHandler.isServerConfiguredToUploadCrashDetails()) {
				LOGGER.info(
						"*****************************************************************************************");
				LOGGER.info(
						"STEP 11:DESCRIPTION: Poll and check for S3 Amazon upload success log message in core_log.txt.");
				LOGGER.info(
						"STEP 11:ACTION: Execute command:grep -i 'S3 minidump Upload is successful with TLS1.2' /rdklogs/logs/core_log.txt.");
				LOGGER.info("STEP 11:EXPECTED -S3 Amazon upload success log message is present");
				LOGGER.info(
						"*****************************************************************************************");

				startTime = System.currentTimeMillis();
				do {
					response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
							BroadBandTraceConstants.LOG_MESSAGE_MINIDUMP_AMAZON_UPLOAD_SUCCESS,
							CrashConstants.LOG_FILE_FOR_CRASHES_RDKB);
					result = CommonMethods.isNotNull(response);
					if (result) {
						break;
					}
				} while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.THREE_MINUTE_IN_MILLIS
						&& BroadBandCommonUtils.hasWaitForDuration(tapEnv,
								BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
				if (result) {
					LOGGER.info("STEP 11: ACTUAL : S3 Amazon upload success log message is present");
				} else {
					LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
				}
				LOGGER.info(
						"*****************************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, step, result, errorMessage, false);
			} else {
				LOGGER.info("server is not configured in RPi");
				tapEnv.updateExecutionForAllStatus(device, testId, step, ExecutionStatus.NOT_APPLICABLE, errorMessage,
						false);
			}

			/*
			 * Step 12: Check if minidump folder has been cleaned up and the log messages
			 * present.
			 */
			step = "s12";
			result = false;
			if (!DeviceModeHandler.isRPIDevice(device)) {
				LOGGER.info(
						"*****************************************************************************************");
				LOGGER.info(
						"STEP 12:DESCRIPTION: Check if minidump folder has been cleaned up and the log messages present.");
				LOGGER.info(
						"STEP 12:ACTION: Execute commands:grep -i 'Cleanup minidump directory /minidumps' /rdklogs/logs/core_log.txt.");
				LOGGER.info("STEP 12:EXPECTED -The minidump folder is empty and log messages are present");
				LOGGER.info(
						"*****************************************************************************************");
				messageList.clear();
				messageList.add(BroadBandTraceConstants.LOG_MESSAGE_MINIDUMP_CLEANUP_DIRECTORY);
				messageList.add(BroadBandTraceConstants.LOG_MESSAGE_MINIDUMP_WORKING_DIR_EMPTY);
				startTime = System.currentTimeMillis();
				do {
					resValues = BroadBandCommonUtils.checkFileForPatterns(device, tapEnv, messageList,
							CrashConstants.LOG_FILE_FOR_CRASHES_RDKB, true);
					errorMessage = resValues.getMessage();
					result = resValues.isResult();
					if (result) {
						break;
					}
				} while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.THREE_MINUTE_IN_MILLIS
						&& BroadBandCommonUtils.hasWaitForDuration(tapEnv,
								BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
				if (result) {
					LOGGER.info("STEP 12: ACTUAL : The minidump folder is empty and log messages are present");
				} else {
					LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
				}
				LOGGER.info(
						"*****************************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, step, result, errorMessage, false);
			} else {
				LOGGER.info("Check if minidump file is created in RPi");
				String pattern = "(.*.dmp)";
				errorMessage = "minidump file not created in RPi";
				response = tapEnv.executeCommandUsingSsh(device,
						BroadBandCommandConstants.CMD_LS + BroadBandTestConstants.STRING_PARTITION_MINIDUMPS);
				LOGGER.info("response from minidumps folder :" + response);
				result = CommonMethods.patternMatcher(response, pattern);
				if (result) {
					LOGGER.info("STEP 7: ACTUAL : The minidump is created in RPi");
				} else {
					LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
				}
				LOGGER.info(
						"*****************************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, step, result, errorMessage, false);
			}

		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("Exception occured while validating minidump upload to S3/crash portal");
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, step, result, errorMessage, true);
		}
	}

	/**
	 * Test to Verify mini dump creation and upload to crash portal for the process
	 * PandM
	 * 
	 * @param device The device to be used.
	 * 
	 * @author Styles Managalasseri
	 * @refactor yamini.s
	 * 
	 */

	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-GBPAD-1001")
	public void testVerifyCrashForProcessPandM(Dut device) {

		// Test case id
		String testCaseId = "TC-RDKB-GBPAD-001";

		// STB Process
		StbProcess stbProcess = StbProcess.PANDM;
		String processName = null;
		DeviceProcess stbProcess2 = new DeviceProcess();

		LOGGER.info("STARTING TEST CASE " + testCaseId);
		LOGGER.info("**********************************************************************************");
		LOGGER.info("Generate crash by restarting the process " + stbProcess.getProcessName()
				+ " within the STB and verify the generated crash file is uploaded in crash portal");
		LOGGER.info("**********************************************************************************");

		generateCrashAndVerify(device, stbProcess, CrashType.MINIDUMP, testCaseId, stbProcess2);

		LOGGER.info("ENDING TEST CASE: TC-RDKB-GBPAD-1001");
		LOGGER.info("#######################################################################################");
	}

	/**
	 * Test to Verify mini dump creation and upload to crash portal for the process
	 * CcspPsm
	 * 
	 * @param device The device to be used.
	 * 
	 * @author Styles Managalasseri
	 * @refactor yamini.s
	 */
	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-GBPAD-1005")
	public void testVerifyCrashForProcessCcspPsm(Dut device) {

		// Test case id
		String testCaseId = "TC-RDKB-GBPAD-005";

		// STB Process
		StbProcess stbProcess = StbProcess.CCSP_PSM;
		String processName = null;
		DeviceProcess stbProcess2 = new DeviceProcess();

		LOGGER.info("STARTING TEST CASE " + testCaseId);
		LOGGER.info("**********************************************************************************");
		LOGGER.info("Generate crash by restarting the process " + stbProcess.getProcessName()
				+ " within the STB and verify the generated crash file is uploaded in crash portal");
		LOGGER.info("**********************************************************************************");

		generateCrashAndVerify(device, stbProcess, CrashType.MINIDUMP, testCaseId, stbProcess2);

		LOGGER.info("ENDING TEST CASE: TC-RDKB-GBPAD-1005");
		LOGGER.info("#######################################################################################");
	}

	/**
	 * Test to Verify mini dump creation and upload to crash portal for the process
	 * webpa
	 * 
	 * @param device The device to be used.
	 * 
	 * @author Styles Managalasseri
	 * @refactor yamini.s
	 */
	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-GBPAD-1007")
	public void testVerifyCrashForProcessWebpa(Dut device) {
		// Test case id
		String testCaseId = "TC-RDKB-GBPAD-007";
		// STB Process
		StbProcess stbProcess = StbProcess.WEBPA;
		String processName = null;
		DeviceProcess stbProcess2 = new DeviceProcess();
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-GBPAD-1007");
		generateCrashAndVerify(device, stbProcess, CrashType.MINIDUMP, testCaseId, stbProcess2);
		LOGGER.info("ENDING TEST CASE: TC-RDKB-GBPAD-1007");
		LOGGER.info("#######################################################################################");
	}

	/**
	 * Test to Verify mini dump creation and upload to crash portal for the process
	 * CcspWifiAgent
	 * 
	 * @param device The device to be used.
	 * 
	 * @author Styles Managalasseri
	 * @refactor yamini.s
	 */
	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-GBPAD-1008")
	public void testVerifyCrashForProcessCcspWifiAgent(Dut device) {
		// Test case id
		String testCaseId = "TC-RDKB-GBPAD-008";
		// STB Process
		StbProcess stbProcess = StbProcess.CCSP_WIFI_AGENT;
		String processName = null;
		DeviceProcess stbProcess2 = new DeviceProcess();
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-GBPAD-1008");
		generateCrashAndVerify(device, stbProcess, CrashType.MINIDUMP, testCaseId, stbProcess2);
		LOGGER.info("ENDING TEST CASE: TC-RDKB-GBPAD-1008");
		LOGGER.info("#######################################################################################");
	}

	/**
	 * Test to Verify mini dump creation and upload to crash portal for the process
	 * meshAgent
	 * 
	 * @param device The device to be used.
	 * 
	 * @author Styles Managalasseri
	 * @refactor yamini.s
	 */
	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-GBPAD-1011")
	public void testVerifyCrashForProcessMeshAgent(Dut device) {

		// Test case id
		String testCaseId = "TC-RDKB-GBPAD-011";

		// STB Process
		StbProcess stbProcess = StbProcess.MESH_AGENT;
		String processName = null;
		DeviceProcess stbProcess2 = new DeviceProcess();

		LOGGER.info("STARTING TEST CASE " + testCaseId);
		LOGGER.info("**********************************************************************************");
		LOGGER.info("Generate crash by restarting the process " + stbProcess.getProcessName()
				+ " within the STB and verify the generated crash file is uploaded in crash portal");
		LOGGER.info("**********************************************************************************");

		generateCrashAndVerify(device, stbProcess, CrashType.MINIDUMP, testCaseId, stbProcess2);

		LOGGER.info("ENDING TEST CASE: TC-RDKB-GBPAD-1011");
		LOGGER.info("#######################################################################################");

	}

	/**
	 * Test to Verify mini dump creation and upload to crash portal for the process
	 * parodus
	 * 
	 * @param device The device to be used.
	 * 
	 * @author Ashwin sankara
	 * @refactor yamini.s
	 */
	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-GBPAD-1012")
	public void testVerifyCrashForProcessParodus(Dut device) {

		// Test case id
		String testCaseId = "TC-RDKB-GBPAD-012";

		// STB Process
		StbProcess stbProcess = StbProcess.PARODUS;
		String processName = null;
		DeviceProcess stbProcess2 = new DeviceProcess();

		LOGGER.info("STARTING TEST CASE " + testCaseId);
		LOGGER.info("**********************************************************************************");
		LOGGER.info("Generate crash by restarting the process " + stbProcess.getProcessName()
				+ " within the STB and verify the generated crash file is uploaded in crash portal");
		LOGGER.info("**********************************************************************************");

		generateCrashAndVerify(device, stbProcess, CrashType.MINIDUMP, testCaseId, stbProcess2);

		LOGGER.info("ENDING TEST CASE: TC-RDKB-GBPAD-1012");
		LOGGER.info("#######################################################################################");

	}

	/**
	 * /** Method to generate crash for given process within the STB and verify the
	 * process in STB
	 * 
	 * @param tapApi               instance of {@link AutomaticsTapApi}
	 * @param device               instance of {@link Dut}
	 * @param process              STB Process type to generate crash and verify
	 * @param crashType            instance of {@link CrashType}
	 * @param testId               Test case ID
	 * 
	 * @param executeInAtomConsole true if need to verify in atom console
	 * @throws TestException during verification failure
	 * 
	 * @refactor yamini.s
	 */
	private void generateCrashAndVerify(Dut device, StbProcess stbProcess, CrashType crashType, String testId,
			DeviceProcess stbProcess2) {
		LOGGER.debug("STARTING METHOD: generateCrashAndVerify()");
		// execution status
		boolean status = false;
		// execute the scripts in ATOM Server
		boolean executeInAtomConsole = false;
		// crash file name which retrieved from crash dump folder
		String crashFileName = null;
		String processName = null;
		// Variable holding test step number
		String testStepNumber = "s1";
		// Variable for holding error message in case of failure
		String errorMessage = null;
		// crash details
		HashMap<String, String> crashDetails = null;
		// instance of SettopCrashUtils
		final SettopCrashUtils settopCrashUtils = new SettopCrashUtils();
		// process ID
		String processId = null;
		// Command to execute
		String command = null;
		// Response for executed command
		String response = null;

		// build id
		String buildId = null;
		// mac address
		String mac = null;
		// device type
		String deviceType = null;
		// device model
		String deviceModel = null;
		// pattern to match the crash file name
		String pattern = null;
		// Temp file created flag
		boolean isTempCoreLogFileCreated = false;

		boolean processKillandVerify = false;
		// Start Time
		long startTime = 0;
		// String to store minidump which uploaded to crash portal
		String logFileUploadedToCrashPortal = null;
		processName = stbProcess.getProcessName();
		stbProcess2.setProcessName(processName);
		try {
			LOGGER.info("TEST DESCRIPTION: Verify " + crashType.getCrashType()
					+ " creation and upload to crash portal for the process " + stbProcess.getProcessName());

			LOGGER.info("TEST STEPS : ");
			LOGGER.info("PRE-CONDITION 1 : Get the process id for " + stbProcess.getProcessName());
			LOGGER.info(
					"PRE-CONDITION 2 : Remove pre-generated crash files and /nvram/automation/core_log.txt file contents");
			LOGGER.info("PRE-CONDITION 3 : Verify the Amazon s3 signing URL in /etc/device.properties");
			LOGGER.info(
					"PRE-CONDITION 4 : Verify the /nvram/coredump.propertie file,If presesnt remove the coredump.propertie file");
			LOGGER.info("1. Kill and veriffy the peocess " + stbProcess.getProcessName());
			LOGGER.info("2. Verify the naming convention of core/mini dump file and Portal URL ");
			LOGGER.info("3. Verify the " + crashType.getCrashType()
					+ " file upload to crash portal in /nvram/automation/core_log.txt.");
			LOGGER.info("4. Verify the " + crashType.getCrashType() + " dump file available in crash portal");
			LOGGER.info("5. Verify the " + crashType.getCrashType() + " dump file processed details in crash portal");
			LOGGER.info("POST-CONDITION 1 : Remove the temporary folder");
			LOGGER.info("#######################################################################################");
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");

			/**
			 * PRE-CONDITION 1 : GET THE PROCESS ID
			 */
			LOGGER.info("#######################################################################################");
			LOGGER.info("PRE-CONDITION 1 : DESCRIPTION : Get the process id for " + stbProcess.getProcessName());
			LOGGER.info("PRE-CONDITION 1 : ACTION : Execute Command : pidof " + stbProcess.getProcessName());
			LOGGER.info("PRE-CONDITION 1 : EXPTECTED : Must return the prrocess id for " + stbProcess.getProcessName());
			LOGGER.info("#######################################################################################");
			errorMessage = "Unable to get the process id for " + stbProcess.getProcessName();
			// retrieve the PID from ARM process
			processId = CommonMethods.getPidOfProcess(device, tapEnv, stbProcess.getProcessName());
			LOGGER.info("Process Id from Arm Console :" + processId);
			if (CommonMethods.isNull(processId) && (CommonMethods.isAtomSyncAvailable(device, tapEnv))) {
				// retrieve the PID form ATOM
				processId = settopCrashUtils.getPidOfProcessFromAtomConsole(device, tapEnv, stbProcess2);
				if (CommonMethods.isNotNull(processId)) {
					executeInAtomConsole = true;
					LOGGER.info(" Process " + stbProcess2.getProcessName() + " is belongs to ATOM :" + processId);
				} else {
					errorMessage = "Unable to identify the process either in ARM or ATOM";
				}
			} else {
				LOGGER.info("Process " + stbProcess2.getProcessName() + " is belongs to ARM");
			}
			status = CommonMethods.isNotNull(processId);
			if (status) {
				LOGGER.info("PRE-CONDITION 1 : ACTUAL : Successfully retrieved the process id for "
						+ stbProcess2.getProcessName());
			} else {
				LOGGER.error("PRE-CONDITION 1 : ACTUAL : " + errorMessage);
				throw new TestException(
						BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION 1 : FAILED : " + errorMessage);
			}

			/**
			 * PRE-CONDITION 2 : REMOVE PRE-GENERATED CRASH FILES AND
			 * /NVRAM/AUTOMATION/CORE_LOG.TXT FILE CONTENTS
			 */
			status = false;
			errorMessage = null;
			LOGGER.info("#######################################################################################");
			LOGGER.info(
					"PRE-CONDITION 2 : DESCRIPTION : Remove pre-generated crash files and create /nvram/automation/core_log.txt file contents");
			LOGGER.info(
					"PRE-CONDITION 2 : ACTION : Execute Command : /rdklogs/logs/core_log.txt;mkdir -p /nvram/automation; > /nvram/automation/core_log.txt'");
			LOGGER.info(
					"PRE-CONDITION 2 : EXPTECTED : Should remove pre-generated crash files create and /nvram/automation/core_log.txt file contents");
			LOGGER.info("#######################################################################################");
			errorMessage = "Unable to remove pre-generated crash files and /nvram/automation/core_log.txt file contents";
			try {
				if (executeInAtomConsole) {
					LOGGER.info("Create the /nvram/automation/core_log.txt in Atom Console");
					settopCrashUtils.perfomPreConditionForCrashVerificationAtomConsole(tapEnv, device);
					BroadBandResultObject resultObject = BroadBandCommonUtils.doesFileExistInAtomConsole(device, tapEnv,
							BroadBandCommandConstants.CORE_LOG_FILE_TEMP_PATH);
					status = resultObject.isStatus();
					errorMessage = resultObject.getErrorMessage();
				} else {
					if (!DeviceModeHandler.isRPIDevice(device)) {
						LOGGER.info("Create the /nvram/automation/core_log.txt in Arm Console");
						settopCrashUtils.perfomPreConditionForCrashVerification(tapEnv, device);
						status = CommonUtils.isFileExists(device, tapEnv,
								BroadBandCommandConstants.CORE_LOG_FILE_TEMP_PATH);
					} else {
						LOGGER.info("removing previously generated dump files");
						String result = tapEnv.executeCommandUsingSsh(device,
								BroadBandTestConstants.CMD_REMOVE_FORCEFULLY
										+ BroadBandTestConstants.STRING_PARTITION_MINIDUMPS + "/"
										+ BroadBandTestConstants.ASTERISK);
						result = tapEnv.executeCommandUsingSsh(device,
								BroadBandCommandConstants.CMD_LS + BroadBandTestConstants.SINGLE_SPACE_CHARACTER
										+ BroadBandTestConstants.STRING_PARTITION_MINIDUMPS);
						if (!result.contains(BroadBandTestConstants.NO_SUCH_FILE_OR_DIRECTORY)) {
							if (CommonMethods.isNull(result)) {
								status = true;
							}
						} else {
							tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_MKDIR
									+ BroadBandTestConstants.STRING_PARTITION_MINIDUMPS);
							status = true;
						}
					}
				}
			} catch (TestException testException) {
				errorMessage = testException.getMessage();
			}
			if (status) {
				if (!DeviceModeHandler.isRPIDevice(device)) {
					isTempCoreLogFileCreated = status;
				}
				LOGGER.info(
						"PRE-CONDITION 2 : ACTUAL : Successfully removed pre-generated crash files and created /nvram/automation/core_log.txt file contents");
			} else {
				LOGGER.error("PRE-CONDITION 2 : ACTUAL : " + errorMessage);
				throw new TestException(
						BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION 2 : FAILED : " + errorMessage);
			}

			/**
			 * PRE-CONDITION 3 : VERIFY THE AMAZON S3 SIGNING URL IN RESPECTIVE PROPERTY
			 * FILE IN /ETC/DEVICE.PROPERTIES
			 */
			status = false;
			errorMessage = null;
			if (BroadbandPropertyFileHandler.isServerConfiguredToUploadCrashDetails()) {
				LOGGER.info("#######################################################################################");
				LOGGER.info(
						"PRE-CONDITION 3 : DESCRIPTION : Verify the Amazon s3 signing URL in respective property file in /etc/device.properties");
				LOGGER.info(
						"PRE-CONDITION 3 : ACTION : Execute Command : grep -i \"S3_AMAZON_SIGNING_URL\" /etc/device.properties");
				LOGGER.info("PRE-CONDITION 3 : EXPTECTED : Should return the proper amazon s3 singing URL");
				LOGGER.info("#######################################################################################");
				errorMessage = "Unable to verify the amazon s3 singing URL";
				String searchText = BroadBandTestConstants.PROP_KEY_AMAZON_URL;
				try {
					command = BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.GREP_COMMAND,
							BroadBandTestConstants.TEXT_DOUBLE_QUOTE, searchText,
							BroadBandTestConstants.TEXT_DOUBLE_QUOTE, BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
							BroadBandCommandConstants.FILE_DEVICE_PROPERTIES);
					response = BroadBandCommonUtils.executeCommandInAtomConsoleIfAtomIsPresentElseInArm(device, tapEnv,
							command);
					status = CommonMethods.isNotNull(response) && CommonMethods.patternMatcher(response.trim(),
							AutomaticsPropertyUtility.getProperty(BroadBandTestConstants.PROP_KEY_S3_AMAZON_URL));
				} catch (Exception exception) {
					status = false;
					errorMessage = exception.getMessage();
				}
				if (status) {
					LOGGER.info("PRE-CONDITION 3 : ACTUAL : Successfully verified the amazon s3 singing URL");
				} else {
					LOGGER.error("PRE-CONDITION 3 : ACTUAL : " + errorMessage);
					throw new TestException(
							BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION 3 : FAILED : " + errorMessage);
				}
			}

			/**
			 * PRE-CONDITION 4 : VERIFY THE /NVRAM/COREDUMP.PROPERTIES FILE,IF PRESESNT
			 * REMOVE THE COREDUMP.PROPERTIES FILE
			 */
			status = false;
			errorMessage = null;
			LOGGER.info("#######################################################################################");
			LOGGER.info(
					"PRE-CONDITION 4 : DESCRIPTION : Verify the /nvram/coredump.properties file,If presesnt remove the coredump.properties file");
			LOGGER.info(
					"PRE-CONDITION 4 : ACTION : Execute Command : if [ -f /nvram/coredump.properties ] ; then echo \"true\" ; else echo \"false\" ; fi");
			LOGGER.info(
					"PRE-CONDITION 4 : EXPTECTED : File coredump.properties should not be present in /nvram/ directory");
			LOGGER.info("#######################################################################################");
			errorMessage = "Unable to remove the /nvram/coredump.properties file";
			try {
				if (executeInAtomConsole) {
					LOGGER.info("Removing the coredump.properties in Atom Console");
					BroadBandResultObject resultObject = BroadBandCommonUtils.doesFileExistInAtomConsole(device, tapEnv,
							BroadBandCommandConstants.FILE_NVRAM_COREDUMP_PROPERTIES);
					status = !resultObject.isStatus();
					errorMessage = resultObject.getErrorMessage();
					if (!status) {
						errorMessage = "File was not removed successfully in Atom Console";
						BroadBandCommonUtils.deleteFileAndVerifyIfAtomPresentElseArm(tapEnv, device,
								BroadBandCommandConstants.FILE_NVRAM_COREDUMP_PROPERTIES, executeInAtomConsole);
						resultObject = BroadBandCommonUtils.doesFileExistInAtomConsole(device, tapEnv,
								BroadBandCommandConstants.FILE_NVRAM_COREDUMP_PROPERTIES);
						status = !resultObject.isStatus();
						errorMessage = errorMessage + resultObject.getErrorMessage();
					}
				} else {
					LOGGER.info("Removing the coredump.properties in Arm Console");
					status = !CommonUtils.isFileExists(device, tapEnv,
							BroadBandCommandConstants.FILE_NVRAM_COREDUMP_PROPERTIES);
					if (!status) {
						errorMessage = "File was not removed successfully in Arm Console";
						CommonUtils.removeFileandVerifyFileRemoval(tapEnv, device,
								BroadBandCommandConstants.FILE_NVRAM_COREDUMP_PROPERTIES);
						status = !CommonUtils.isFileExists(device, tapEnv,
								BroadBandCommandConstants.FILE_NVRAM_COREDUMP_PROPERTIES);
					}
				}
			} catch (Exception exception) {
				errorMessage = exception.getMessage();
			}
			if (status) {
				LOGGER.info(
						"PRE-CONDITION 4 : ACTUAL : Successfully removed the coredump.properties file in /nvram/ directory");
			} else {
				LOGGER.error("PRE-CONDITION 4 : ACTUAL : " + errorMessage);
				throw new TestException(
						BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION 4 : FAILED : " + errorMessage);
			}
			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

			/**
			 * SETP 1: KILL AND VERIFY THE PROCESS.VERIFY THE CRASH FILE IN CRASH FOLDER
			 * LOCATION
			 */

			status = false;
			boolean webpaLastRebootStatus = false;
			boolean crashFileStatus = false;
			errorMessage = null;
			String lastRebootReson = null;
			testStepNumber = "s1";
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 1 : DESCRIPTION : Kill and verify the process " + stbProcess.getProcessName()
					+ ". Verify the crash file in crash folder location");
			LOGGER.info("STEP 1 : ACTION : Execute Command : killall -11 " + stbProcess.getProcessName()
					+ " or kill -11 \"" + stbProcess.getProcessName() + "\"");
			LOGGER.info("STEP 1 : EXPECTED : " + stbProcess.getProcessName()
					+ " process should kill and restart with new PID. " + crashType.getCrashType()
					+ " file should be created in crash folder location.");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to kill and verify the " + stbProcess.getProcessName() + " process.";

			if (stbProcess.getProcessName().equals(BroadBandCommandConstants.CCSP_COMPONENT_PROCESS_NAME)) {

				LOGGER.info("VALIDATING " + stbProcess.getProcessName() + " PROCESS");

				try {

					lastRebootReson = tapEnv.executeWebPaCommand(device,
							BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_LAST_REBOOT_REASON);
					LOGGER.info("VERFIYING LAST REBOOT RESASON BEFORE CR_CRASH RETRIEVED FROM WEBPA GET IS : "
							+ lastRebootReson);
					// restart STB process
					if (executeInAtomConsole) {
						crashDetails = settopCrashUtils.restartProcessInAtomConsole(device, tapEnv, stbProcess2,
								crashType);
					} else {
						if (!DeviceModeHandler.isRPIDevice(device)) {
							crashDetails = settopCrashUtils.restartProcessInArmConsole(device, tapEnv, stbProcess2,
									crashType);
						} else {

							LOGGER.info("Device level Validation only required for RPi currently :");
							processKillandVerify = BroadBandCommonUtils.killProcessAndVerifyForCrashGeneration(device,
									tapEnv, stbProcess2.getProcessName());
							if (processKillandVerify) {
								LOGGER.info("process killed successfully ");
							}
						}
					}

					// retrieve crash file name
					crashFileName = crashDetails.get(SettopCrashUtils.PROPERTY_KEY_CRASH_FILE_NAME);
					// verify whether given crash file is valid or not
					LOGGER.info("crashfilename :" + crashFileName);
					crashFileStatus = CommonMethods.isNotNull(crashFileName);

					startTime = System.currentTimeMillis();
					if (!DeviceModeHandler.isRPIDevice(device)) {
						do {
							lastRebootReson = tapEnv.executeWebPaCommand(device,
									BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_LAST_REBOOT_REASON);
							LOGGER.info("VERFIYING LAST REBOOT RESASON AFTER CR_CRASH RETRIEVED FROM WEBPA GET IS : "
									+ lastRebootReson);
							webpaLastRebootStatus = CommonMethods.isNotNull(lastRebootReson) && CommonMethods
									.patternMatcher(lastRebootReson, BroadBandTestConstants.STRING_CR_CRASH);

						} while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS
								&& !webpaLastRebootStatus && BroadBandCommonUtils.hasWaitForDuration(tapEnv,
										BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
					}

				} catch (TestException testException) {
					errorMessage = testException.getMessage();
				}
				status = crashFileStatus && webpaLastRebootStatus;

			} else {
				LOGGER.info("VALIDATING " + stbProcess.getProcessName() + " PROCESS");

				try {
					// restart STB process
					if (executeInAtomConsole) {
						crashDetails = settopCrashUtils.restartProcessInAtomConsole(device, tapEnv, stbProcess2,
								crashType);
					} else {
						if (!DeviceModeHandler.isRPIDevice(device)) {
							crashDetails = settopCrashUtils.restartProcessInArmConsole(device, tapEnv, stbProcess2,
									crashType);
						} else {

							LOGGER.info("Device level Validation only required for RPi currently :");
							processKillandVerify = BroadBandCommonUtils.killProcessAndVerifyForCrashGeneration(device,
									tapEnv, stbProcess2.getProcessName());
							if (processKillandVerify) {
								LOGGER.info("process killed successfully ");
							}
						}
					}
					// retrieve crash file name
					if (!DeviceModeHandler.isRPIDevice(device)) {
						crashFileName = crashDetails.get(SettopCrashUtils.PROPERTY_KEY_CRASH_FILE_NAME);
						LOGGER.info("CRASHFILENAME :" + crashFileName);
						// verify whether given crash file is valid or not
						status = CommonMethods.isNotNull(crashFileName);
						errorMessage = "Unable to retrieve crash file from crash logs";
					} else {
						LOGGER.info("is process killed : " + processKillandVerify);
						if (processKillandVerify) {
							status = true;
							errorMessage = "Unable to kill the process ";
						}
					}
				} catch (TestException testException) {
					errorMessage = testException.getMessage();
				}
			}
			if (status) {
				// Added minidump file name rewritten with upload time
				if (executeInAtomConsole) {
					response = BroadBandCommonUtils.searchLogFilesInAtomConsoleByPolling(tapEnv, device,
							BroadBandTestConstants.STRING_FOR_CRASH_PORTAL_FILE_NAME_SEARCH,
							BroadBandCommandConstants.CORE_LOG_FILE_TEMP_PATH,
							BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
							BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);

				} else {
					if (!DeviceModeHandler.isRPIDevice(device)) {
						response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
								BroadBandTestConstants.STRING_FOR_CRASH_PORTAL_FILE_NAME_SEARCH,
								BroadBandCommandConstants.CORE_LOG_FILE_TEMP_PATH,
								BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
								BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
					}
				}
				if (CommonMethods.isNotNull(response)) {
					logFileUploadedToCrashPortal = BroadBandCommonUtils.patternFinderMatchPositionBased(response,
							CommonUtils.concatStringUsingStringBuffer(
									BroadBandTestConstants.STRING_FOR_CRASH_PORTAL_FILE_NAME_SEARCH,
									BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
									BroadBandTestConstants.STRING_REGEX_WILDCARD),
							BroadBandTestConstants.CONSTANT_1);
					// verify whether given crash file is valid or not

					status = CommonMethods.isNotNull(logFileUploadedToCrashPortal);
				}
			}
			if (status) {
				if (!DeviceModeHandler.isRPIDevice(device)) {
					LOGGER.info("STEP 1 : ACTUAL : Successfully retrieved the crash file name as: '" + crashFileName
							+ "' and renamed to: " + logFileUploadedToCrashPortal);
				} else {
					LOGGER.info("STEP 1 : ACTUAL : Successfully verified if the process is killed in RPi");
				}
			} else {
				LOGGER.error("STEP 1 : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * SETP 2: VERIFY THE NAMING CONVENTION OF CORE/MINI DUMP FILE AND PORTAL URL
			 */
			status = false;
			errorMessage = null;
			testStepNumber = "s2";
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 2 : DESCRIPTION : Verify the naming convention of " + crashType.getCrashType()
					+ " dump file created by " + stbProcess.getProcessName() + " process");
			LOGGER.info("STEP 2 : ACTION : Extract core/mini dump file name from /nvram/automation/core_log.txt");
			LOGGER.info(
					"STEP 2 : EXPECTED : Name of core/mini dump file should be following format - <buildID>_mac<erouter mac address>_dat<current_date>_mod<model name>_<file name>.dmp");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to verify the naming convention of " + crashType.getCrashType()
					+ " dump file created by " + stbProcess.getProcessName() + " process";
			try {
				// retrieve mac address
				mac = getSettopMacAddressForCrashDetailsValidation(device, tapEnv, executeInAtomConsole);
				// retrieve device type
				if (!DeviceModeHandler.isRPIDevice(device)) {
					deviceType = settopCrashUtils.getDeviceTypeForCrashDetailsVerfication(device, tapEnv);
				}
				// retrieve device model
				deviceModel = device.getModel();

				LOGGER.info("Obtained device model name : " + deviceModel);
				// retrieve build id
				if (!DeviceModeHandler.isRPIDevice(device)) {
					buildId = crashDetails.get(SettopCrashUtils.PROPERTY_KEY_BUILD_ID);
					if (CommonMethods.isNotNull(buildId) && CommonMethods.isNotNull(mac)
							&& CommonMethods.isNotNull(deviceType) && CommonMethods.isNotNull(deviceModel)) {

						if (DeviceModeHandler.isFibreDevice(device)) {
							LOGGER.info("Obtained device model name for fiber device : " + deviceModel);
							pattern = buildId + "_mac" + mac + "_dat\\d+-\\d+-\\d+-\\d+-\\d+-\\d+" + "_" + deviceType
									+ "_mod[P]*" + deviceModel + "_.*" + ".dmp.tgz";
						}

						else if (BroadbandPropertyFileHandler.isDeviceCheckForGBPAD(device)) {
							deviceModel = deviceModel.substring(0, deviceModel.length() - 1);
							LOGGER.info("Obtained device model name for device : " + deviceModel);
							pattern = buildId + "_mac" + mac + "_dat\\d+-\\d+-\\d+-\\d+-\\d+-\\d+" + "_" + deviceType
									+ "_mod" + deviceModel + ".*_.*" + ".dmp.tgz";
						}

						else if (BroadbandPropertyFileHandler.isDeviceCheckForGBPAD1(device)) {

							if (BroadbandPropertyFileHandler.isDeviceCheckForGBPAD2(device)) {
								deviceModel = deviceModel + "B";
								LOGGER.info("Obtained device model name for business class device : " + deviceModel);
							} else {
								LOGGER.info("Obtained device model name for device : " + deviceModel);
							}

							pattern = buildId + "_mac" + mac + "_dat\\d+-\\d+-\\d+-\\d+-\\d+-\\d+" + "_" + deviceType
									+ "_mod" + deviceModel + ".*_.*" + ".dmp.tgz";

						} else if (DeviceModeHandler.isDSLDevice(device)) {

							pattern = buildId + "_mac" + mac + "_dat\\d+-\\d+-\\d+-\\d+-\\d+-\\d+" + "_" + deviceType
									+ "_mod" + deviceModel + ".*_.*" + ".dmp.tgz";

						} else {
							pattern = buildId + "_mac" + mac + "_dat\\d+-\\d+-\\d+-\\d+-\\d+-\\d+" + "_" + deviceType
									+ "_mod" + deviceModel + "_.*" + ".dmp.tgz";
						}
						status = CommonMethods.patternMatcher(crashFileName, pattern)
								&& CommonMethods.patternMatcher(logFileUploadedToCrashPortal, pattern);
						;
						errorMessage = "The naming format of the crash file is not in expected format. ie, format="
								+ pattern + " and filename = " + crashFileName;
					} else {
						errorMessage = "not able to retrieve all the parameters for the crash file format validation. ie, buildId="
								+ buildId + ", mac=" + mac + ", deviceType=" + deviceType + ", deviceModel="
								+ deviceModel;
					}
				} else {
					pattern = "(.*.dmp)";
					response = tapEnv.executeCommandUsingSsh(device,
							BroadBandCommandConstants.CMD_LS + BroadBandTestConstants.STRING_PARTITION_MINIDUMPS);
					LOGGER.info("response from minidumps folder :" + response);
					if (CommonMethods.isNull(response)) {
						BroadBandCommonUtils.rebootAndWaitForStbAccessible(device, tapEnv);
						response = tapEnv.executeCommandUsingSsh(device,
								BroadBandCommandConstants.CMD_LS + BroadBandTestConstants.STRING_PARTITION_MINIDUMPS);
						LOGGER.info("response from minidumps folder :" + response);
					}
					status = CommonMethods.patternMatcher(response, pattern);
				}
			} catch (TestException testException) {
				errorMessage = errorMessage + testException.getMessage();
			}
			if (status) {
				if (!DeviceModeHandler.isRPIDevice(device)) {
					LOGGER.info("STEP 2 : ACTUAL : Successfully verified the naming converntion for "
							+ crashType.getCrashType() + " dump file created by " + stbProcess.getProcessName()
							+ " process");
				} else {
					LOGGER.info("STEP 2 : ACTUAL : Successfully verified minidumps file created by "
							+ stbProcess.getProcessName() + " process. Dumpfile : " + response);
				}
			} else {
				LOGGER.error("STEP 2 : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			if (BroadbandPropertyFileHandler.isServerConfiguredToUploadCrashDetails()) {
				/**
				 * SETP 3: VERIFY THE CORE/MINI DUMP FILE UPLOAD TO CRASH PORTAL IN
				 * /NVRAM/AUTOMATION/CORE_LOG.TXT.
				 */
				status = false;
				errorMessage = null;
				testStepNumber = "s3";
				// variable to store crash file upload success string
				String crashFileUploadSuccessString = null;
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP 3 : DESCRIPTION : Verify the " + crashType.getCrashType()
						+ " file upload to crash portal in /nvram/automation/core_log.txt.");
				LOGGER.info(
						"STEP 3 : ACTION : Check core/mini dump file uploaded to crash portal in� /nvram/automation/core_log.txt");
				LOGGER.info(
						"STEP 3 : EXPECTED : logs with crash portal upload success should be present in /nvram/automation/core_log.txt file");
				LOGGER.info("**********************************************************************************");
				errorMessage = "Unable to verify the " + crashType.getCrashType()
						+ " file upload to crash portal in /nvram/automation/core_log.txt.";
				try {
					// retrieve crash file upload success string from crash details
					crashFileUploadSuccessString = crashDetails
							.get(SettopCrashUtils.PROPERTY_KEY_CRASH_FILE_UPLOAD_SUCCESS);
					LOGGER.info("Crash file Upload Success String : " + crashFileUploadSuccessString);
					if (CommonMethods.isNotNull(crashFileUploadSuccessString)) {
						if (crashType.equals(CrashType.COREDUMP)) {
							status = crashFileUploadSuccessString
									.equals(SettopCrashUtils.CORE_DUMP_UPLOAD_SUCCESS_STRING);
							errorMessage = "Unable to verify the presence of "
									+ SettopCrashUtils.CORE_DUMP_UPLOAD_SUCCESS_STRING + " log in core_log.txt";
						} else if (crashType.equals(CrashType.MINIDUMP)) {
							status = crashFileUploadSuccessString
									.equals(SettopCrashUtils.MINI_DUMP_UPLOAD_SUCCESS_STRING);
							errorMessage = "Unable to verify the presence of "
									+ SettopCrashUtils.MINI_DUMP_UPLOAD_SUCCESS_STRING + " log in core_log.txt";
						}
					} else {
						errorMessage = "Unable to retrieve success string log from core_log.txt";
					}
					if (!status) {
						status = CommonMethods.isNotNull(CommonMethods.caseInsensitivePatternFinder(
								crashFileUploadSuccessString, BroadBandTestConstants.FAIL_OVER_MECHANISM_VBN_SUCCESS));
						LOGGER.info("Upload to crashportal Status : " + status);
						if (status) {
							LOGGER.info("Verified upload to crashportal eventhough upload to S3 failed");
						}
					}
				} catch (TestException testException) {
					errorMessage = testException.getMessage();
				}
				if (status) {
					LOGGER.info(
							"STEP 3 : ACTUAL : Successfully verified that the crash file is uploaded into crash portal");
				} else {
					LOGGER.error("STEP 3 : ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

				/**
				 * SETP 4: verify the crash file in crash portal
				 */
				status = false;
				errorMessage = null;
				testStepNumber = "s4";
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP 4 : DESCRIPTION : Verify the " + crashType.getCrashType()
						+ " dump file available in crash portal");
				LOGGER.info("STEP 4 : ACTION : Check core/mini dump file available in crash portal");
				LOGGER.info("STEP 4 : EXPECTED : " + crashType.getCrashType()
						+ " dump file should be available in Crash portal");
				LOGGER.info("**********************************************************************************");
				errorMessage = "Unable to verify " + crashType.name() + " crash file '" + logFileUploadedToCrashPortal
						+ "' in crash portal for the device " + device.getHostMacAddress();
				startTime = System.currentTimeMillis();
				SettopCrashUtils settopCrashUtils2 = new SettopCrashUtils();
				// verify crash in crash portal
				status = settopCrashUtils2.verifyCrashFileInCrashPortal(tapEnv, device, crashType,
						logFileUploadedToCrashPortal);
				if (status) {
					LOGGER.info(
							"STEP 4 : ACTUAL : Successfully verified that the crash file is available in crash portal");
				} else {
					LOGGER.error("STEP 4 : ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

				/**
				 * SETP 5: verify the crash file processed details in crash portal
				 */
				status = false;
				errorMessage = null;
				testStepNumber = "s5";
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP 5 : DESCRIPTION : Verify the " + crashType.getCrashType()
						+ " dump file processed details in crash portal");
				LOGGER.info(
						"STEP 5 : ACTION : Check whether core/mini dump properly processed in Crash portal using REST API.");
				LOGGER.info("STEP 5 : EXPECTED : " + crashType.getCrashType()
						+ " dump file should process successfully and the details should be availabe in Crash portal");
				LOGGER.info("**********************************************************************************");
				errorMessage = "Unable to verify all the properties of crash file details which retrieved from crash portal";
				status = verifyCrashFileDetailsInCrashPortal(tapEnv, device, stbProcess2, crashType,
						logFileUploadedToCrashPortal, executeInAtomConsole, settopCrashUtils, startTime);
				if (status) {
					LOGGER.info("STEP 5 : ACTUAL : Successfully verified crash file processed details in crash portal");
				} else {
					LOGGER.error("STEP 5 : ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			} else {
				LOGGER.info("#######################################################################################");
				LOGGER.info(
						"Steps 3 to 5 are not applicable if server details to upload crash details ate not configured");
				LOGGER.info("#######################################################################################");
				tapEnv.updateExecutionForAllStatus(device, testId, "s3", ExecutionStatus.NOT_APPLICABLE,
						"N/A if server details are not configured", false);
				tapEnv.updateExecutionForAllStatus(device, testId, "s4", ExecutionStatus.NOT_APPLICABLE,
						"N/A if server details are not configured", false);
				tapEnv.updateExecutionForAllStatus(device, testId, "s5", ExecutionStatus.NOT_APPLICABLE,
						"N/A if server details are not configured", false);

			}

		} catch (Exception e) {
			errorMessage = e.getMessage();
			LOGGER.error("Exception occurred while verifying " + crashType.getCrashType()
					+ " creation and upload to crash portal for the process " + stbProcess.getProcessName() + " : "
					+ errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStepNumber, status, errorMessage,
					true);
		} finally {
			if (isTempCoreLogFileCreated) {
				LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
				status = false;
				errorMessage = null;
				LOGGER.info("#######################################################################################");
				LOGGER.info("POST-CONDITION 1 : DESCRIPTION : Remove the /nvram/automation/core_log.txt ");
				LOGGER.info("POST-CONDITION 1 : ACTION : Execute Command : rm -rf /nvram/automation/core_log.txt");
				LOGGER.info("POST-CONDITION 1 : EXPTECTED : Should remove the /nvram/automation/core_log.txt");
				LOGGER.info("#######################################################################################");
				errorMessage = "Unable to remove the /nvram/automation/core_log.txt";
				settopCrashUtils.performPostConditionForCrashVerificationTestScripts(device, tapEnv,
						executeInAtomConsole);
				if (executeInAtomConsole) {
					LOGGER.info("POST CONDITION: Verify the /nvram/automation/core_log.txt in Atom Console");
					BroadBandResultObject resultObject = BroadBandCommonUtils.doesFileExistInAtomConsole(device, tapEnv,
							BroadBandCommandConstants.CORE_LOG_FILE_TEMP_PATH);
					status = !resultObject.isStatus();
					errorMessage = resultObject.getErrorMessage();
				} else {
					LOGGER.info("POST CONDITION: Verify the /nvram/automation/core_log.txt in Arm Console");
					status = !CommonUtils.isFileExists(device, tapEnv,
							BroadBandCommandConstants.CORE_LOG_FILE_TEMP_PATH);
				}
				if (status) {
					LOGGER.info("POST-CONDITION 1 : ACTUAL : Successfully removed the /nvram/automation/core_log.txt.");
				} else {
					LOGGER.error("POST-CONDITION 1 : ACTUAL :" + errorMessage);
				}
				LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
			}
		}
	}

	/**
	 * Method to retrieve the STB MAC address for crash details validation
	 * 
	 * @param device instance of {@link Dut}
	 * @return MAC address for crash details validation
	 * 
	 * @refactor yamini.s
	 */
	public String getSettopMacAddressForCrashDetailsValidation(Dut device, AutomaticsTapApi tapApi,
			boolean executeInAtomConsole) {

		LOGGER.debug("STARTING METHOD: getSettopMacAddressForCrashDetailsValidation()");
		// MAC Address
		String mac = null;
		// server response
		String response = null;
		// Regular expression to retrieve MAC Address from interface details
		String regexForMacAddress = "HWaddr\\s*(" + BroadBandTestConstants.REG_EX_MAC_ADDRESS_FORMAT + ")";
		String regexForMacAddressInRdkV = "(" + BroadBandTestConstants.REG_EX_MAC_ADDRESS_FORMAT + ")";

		if (SupportedModelHandler.isRDKB(device)) {
			// by default the interface name is erouter0
			response = tapApi.executeCommandUsingSsh(device,
					"/sbin/ifconfig " + DEFAULT_INTERFACE_NAME_TO_RETRIEVE_ESTB_MAC_ADDRESS);
			LOGGER.info("Response for /sbin/ifconfig : " + response);
		} else if (SupportedModelHandler.isRDKV(device)) {
			// for rdkv boxes get estb mac
			response = CommonMethods.getSTBDetails(device, tapApi, StbDetails.ESTB_MAC);
			LOGGER.info("Response for getSTBDetails : " + response);
		} else if (SupportedModelHandler.isRDKC(device)) {
			response = tapApi.executeCommandUsingSsh(device, " /sbin/ifconfig | grep HWaddr");
			LOGGER.info("Response for /sbin/ifconfig : " + response);
		}

		if (CommonMethods.isNotNull(response)) {
			if (SupportedModelHandler.isRDKB(device) || SupportedModelHandler.isRDKC(device)) {
				mac = CommonMethods.patternFinder(response, regexForMacAddress);
			} else if (SupportedModelHandler.isRDKV(device)) {
				mac = CommonMethods.patternFinder(response, regexForMacAddressInRdkV);
			}
		}
		// format the mac address
		if (CommonMethods.isNotNull(mac)) {
			LOGGER.info("MAC obtained : " + mac);
			mac = mac.replaceAll(AutomaticsConstants.COLON, AutomaticsConstants.EMPTY_STRING);
			mac = mac.toUpperCase();
		}

		LOGGER.info("MAC Address retrieved from the interface is " + mac);

		LOGGER.debug("ENDING METHOD: getSettopMacAddressForCrashDetailsValidation()");

		return mac;
	}

	/**
	 * Method to validate the crash details in crash portal
	 * 
	 * @param tapApi               instance of {@link AutomaticsTapApi}
	 * @param device               instance of {@link Dut}
	 * @param stbProcess           instance of {@link StbProcess}
	 * @param crashType            crash type(mini/core)
	 * @param fileName             file name to verify
	 * @param executeInAtomConsole True -Execute command in Atom console ,Else Arm
	 *                             console
	 * @param settopCrashUtils     instance of {@link SettopCrashUtils}
	 * @param startTime            start Time
	 * @return status True if crash details validation is success else false
	 * @throws Exception if validation failed
	 * 
	 * @refactor yamini.s
	 */
	public static boolean verifyCrashFileDetailsInCrashPortal(AutomaticsTapApi tapApi, Dut device,
			DeviceProcess deviceProcess, CrashType crashType, String fileName, boolean executeInAtomConsole,
			SettopCrashUtils settopCrashUtils, long startTime) throws Exception {
		LOGGER.debug("STARTING METHOD: verifyCrashFileDetailsInCrashPortal()");
		boolean status = false;
		CrashDetails crashDetails = null;

		String errorMessage = "Unable to get processed crash details from crash portal";
		try {
			do {
				// retrieve the crash details from the crash portal
				crashDetails = settopCrashUtils.getCrashFileDetailsInCrashPortal(tapApi, device, crashType, fileName);

				if (crashDetails != null && CommonMethods.isNotNull(crashDetails.getSignature()) && !CommonMethods
						.patternMatcher(crashDetails.getSignature(), BroadBandTestConstants.NOT_PROCESSED)) {
					errorMessage = settopCrashUtils.verifyIndividualCrashDetails(tapApi, device, crashDetails,
							deviceProcess, fileName, executeInAtomConsole);
					if (CommonMethods.isNotNull(errorMessage)) {
						LOGGER.error(errorMessage);
					} else {
						status = true;
					}
				}
			} while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.FIFTEEN_MINUTES_IN_MILLIS
					&& !status
					&& CommonMethods.hasWaitForDuration(tapApi, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS));
		} catch (Exception e) {
			LOGGER.error("Exception Occurred getCrashFileDetailsInCrashPortal() " + e.getMessage());
		}
		LOGGER.debug("ENDING METHOD: verifyCrashFileDetailsInCrashPortal()");
		return status;
	}

	/**
	 * Test to Verify mini dump creation and upload to crash portal for the process
	 * CcspCr
	 * 
	 * @param device The device to be used.
	 * 
	 * @author Styles Managalasseri
	 * 
	 * @refactor yamini.s
	 * 
	 */
	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-GBPAD-1006")
	public void testVerifyCrashForProcessCcspCr(Dut device) {
		// Test case id
		String testCaseId = "TC-RDKB-GBPAD-006";
		String testStepNumber = "s1";

		// STB Process
		StbProcess stbProcess = StbProcess.CCSP_CR;
		String processName = null;
		DeviceProcess stbProcess2 = new DeviceProcess();

		// Disable self heal

		boolean status = false;
		String errorMessage = null;

		try {
			if (BroadBandWebPaUtils.getRbusModeStatus(device, tapEnv, BroadBandTestConstants.BOOLEAN_VALUE_FALSE)) {
				tapEnv.setWebPaParams(device, BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_SELFHEAL_PROCESS_ENABLE_STATUS,
						BroadBandTestConstants.FALSE, WebPaDataTypes.BOOLEAN.getValue());
				LOGGER.info("STARTING TEST CASE " + testCaseId);
				LOGGER.info("**********************************************************************************");
				LOGGER.info("Generate crash by restarting the process " + stbProcess.getProcessName()
						+ " within the STB and verify the generated crash file is uploaded in crash portal");
				LOGGER.info("**********************************************************************************");

				generateCrashAndVerify(device, stbProcess, CrashType.MINIDUMP, testCaseId, stbProcess2);

				// Enable self heal
				tapEnv.setWebPaParams(device, BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_SELFHEAL_PROCESS_ENABLE_STATUS,
						BroadBandTestConstants.TRUE, WebPaDataTypes.BOOLEAN.getValue());
			} else {
				errorMessage = " Rbus mode is in enabled state. so marking the steps as NA";
				int stepNumber = 1;
				while (stepNumber <= 5) {
					testStepNumber = "s" + stepNumber;
					errorMessage = "STEP " + stepNumber + ": " + errorMessage;
					LOGGER.info("**********************************************************************************");
					tapEnv.updateExecutionForAllStatus(device, testCaseId, testStepNumber,
							ExecutionStatus.NOT_APPLICABLE, errorMessage, false);
					stepNumber++;
				}
			}
		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, testStepNumber, status,
					errorMessage, true);
		}
	}

	/**
	 * Test to Verify mini dump creation and upload to crash portal for the process
	 * CcspTr069
	 * 
	 * @param Dut The device to be used.
	 * 
	 * @author Styles Managalasseri, TATA Elxsi
	 * @refactor Alan_Bivera
	 * 
	 */
	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true)
	@TestDetails(testUID = "TC-RDKB-GBPAD-1004")
	public void testVerifyCrashForProcessCcspTr069(Dut device) {

		// Test case id
		String testCaseId = "TC-RDKB-GBPAD-004";

		// STB Process
		StbProcess stbProcess = StbProcess.CCSP_TR069;
		DeviceProcess stbProcess2 = new DeviceProcess();

		boolean tr69Status = BroadBandTr69Utils.checkAndEnableTr69Support(device, tapEnv);
		LOGGER.info("Status of TR69 before starting the test case : " + tr69Status);

		LOGGER.info("STARTING TEST CASE " + testCaseId);
		LOGGER.info("**********************************************************************************");
		LOGGER.info("Generate crash by restarting the process " + stbProcess.getProcessName()
				+ " within the STB and verify the generated crash file is uploaded in crash portal");
		LOGGER.info("**********************************************************************************");

		generateCrashAndVerify(device, stbProcess, CrashType.MINIDUMP, testCaseId, stbProcess2);
	}

	/**
	 * Test to Verify mini dump creation and upload to crash portal for the process
	 * CcspHotspot
	 * 
	 * @param Dut The device to be used.
	 * 
	 * @author Nidhal Shamsudheen
	 * @refactor Alan_Bivera
	 * 
	 */
	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true)
	@TestDetails(testUID = "TC-RDKB-GBPAD-1014")
	public void testVerifyCrashForProcessCcspHotspot(Dut device) {

		// Test case id
		String testCaseId = "TC-RDKB-GBPAD-014";

		// STB Process
		StbProcess stbProcess = StbProcess.CCSP_HOTSPOT;
		DeviceProcess stbProcess2 = new DeviceProcess();

		int preConStepNumber = BroadBandTestConstants.CONSTANT_1;
		boolean status = false;
		String errorMessage = null;
		String response = null;
		int postConStepNumber = BroadBandTestConstants.CONSTANT_1;
		long startTimeStamp = System.currentTimeMillis();

		/**
		 * PRE-CONDITION 1 : Enable Public WiFi in the device
		 */

		LOGGER.info("#######################################################################################");
		LOGGER.info("PRE-CONDITION " + preConStepNumber + " : DESCRIPTION : Enable Public Wi-Fi");
		LOGGER.info("PRE-CONDITION " + preConStepNumber
				+ " : ACTION : Execute WebPA set command to enable Public WiFi - Device.DeviceInfo.X_COMCAST_COM_PublicwifiEnable bool true.");
		LOGGER.info("PRE-CONDITION " + preConStepNumber + " : EXPECTED : Public WiFi should be enabled");
		LOGGER.info("#######################################################################################");

		errorMessage = "Not able to enable Public Wi-Fi";

		status = BroadBandWiFiUtils.checkAndSetPublicWifi(device, tapEnv);

		if (status) {
			LOGGER.info(
					"PRE-CONDITION " + preConStepNumber + " : ACTUAL : Successfully enabled Public WiFi in the device");

		} else {
			LOGGER.error("PRE-CONDITION " + preConStepNumber + " : ACTUAL : " + errorMessage);
			throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION " + preConStepNumber
					+ " : FAILED : " + errorMessage);
		}
		LOGGER.info("#######################################################################################");

		status = false;
		preConStepNumber++;

		LOGGER.info("#######################################################################################");
		LOGGER.info("PRE-CONDITION " + preConStepNumber + " : DESCRIPTION : Verify CcspHotspot process is running");
		LOGGER.info("PRE-CONDITION " + preConStepNumber
				+ " : ACTION : Get PID of CcspHospot using the command - pidof CcspHotspot");
		LOGGER.info("PRE-CONDITION " + preConStepNumber + " : EXPECTED : CcspHospot should be enabled");
		LOGGER.info("#######################################################################################");

		errorMessage = "CcspHotspot is not running!";
		do {
			response = CommonMethods.getPidOfProcess(device, tapEnv, stbProcess.getProcessName());
			status = CommonMethods.isNotNull(response);
		} while (!status
				&& (System.currentTimeMillis() - startTimeStamp) < BroadBandTestConstants.THREE_MINUTE_IN_MILLIS
				&& BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.FIFTEEN_SECONDS_IN_MILLIS));

		if (status) {
			LOGGER.info("PRE-CONDITION " + preConStepNumber + " : ACTUAL : CcspHotspot is running in the device");
		} else {
			LOGGER.error("PRE-CONDITION " + preConStepNumber + " : ACTUAL : " + errorMessage);
			throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION " + preConStepNumber
					+ " : FAILED : " + errorMessage);
		}
		LOGGER.info("#######################################################################################");

		LOGGER.info("STARTING TEST CASE " + testCaseId);
		LOGGER.info("**********************************************************************************");
		LOGGER.info("Generate crash by restarting the process " + stbProcess.getProcessName()
				+ " within the STB and verify the generated crash file is uploaded in crash portal");
		LOGGER.info("**********************************************************************************");

		generateCrashAndVerify(device, stbProcess, CrashType.MINIDUMP, testCaseId, stbProcess2);

		LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
		LOGGER.info("POST-CONDITION STEPS");
		/**
		 * POST-CONDITION 1 : ENABLE/DISABLE THE PUBLIC WIFI based on value set in
		 * rdkb.whitelist.Publicwifivalue property
		 */
		BroadBandPostConditionUtils.executePostConditionToEnableOrDisablePublicWifiBasedOnStbProperty(device, tapEnv,
				postConStepNumber);

		LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");

		LOGGER.info("ENDING TEST CASE: TC-RDKB-GBPAD-1014");

	}

	/**
	 * Test to Verify mini dump creation and upload to crash portal for the process
	 * SNMP_SUBAGENT
	 * 
	 * @param Dut The device to be used.
	 * 
	 * @author Nidhal Shamsudheen
	 * @refactor Alan_Bivera
	 * 
	 */
	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true)
	@TestDetails(testUID = "TC-RDKB-GBPAD-1015")
	public void testVerifyCrashForProcessSnmpSubagent(Dut device) {

		// Test case id
		String testCaseId = "TC-RDKB-GBPAD-015";

		// STB Process
		StbProcess stbProcess = StbProcess.SNMP_SUBAGENT;
		DeviceProcess stbProcess2 = new DeviceProcess();

		LOGGER.info("STARTING TEST CASE " + testCaseId);
		LOGGER.info("**********************************************************************************");
		LOGGER.info("Generate crash by restarting the process " + stbProcess.getProcessName()
				+ " within the STB and verify the generated crash file is uploaded in crash portal");
		LOGGER.info("**********************************************************************************");

		generateCrashAndVerify(device, stbProcess, CrashType.MINIDUMP, testCaseId, stbProcess2);

	}

}
