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

/**
 * 
 * Test class for validating DeviceManagement UploadLogs Parameters
 * 
 * @author Praveen_chandru
 *
 */
package com.automatics.rdkb.tests.uploadlogs;

import java.util.ArrayList;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.error.ErrorType;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandPropertyKeyConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandPostConditionUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpMib;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpUtils;
import com.automatics.rdkb.utils.uploadlogs.BroadBandUploadLogsUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.snmp.SnmpDataType;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;
import com.automatics.rdkb.utils.BroadBandPreConditionUtils;
import com.automatics.rdkb.utils.cdl.BroadBandCodeDownloadUtils;

public class TelemetryLogUploadTest extends AutomaticsTestBase {

	/**
	 * 
	 * 
	 * Verify when setting UploadLogsNow parameter to False, no upload happens
	 *
	 *
	 * <p>
	 * <ol>
	 * <li>S1) Verify retrieving the current value of WebPA
	 * parameter:Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMLogsUploadStatus</li>
	 * <li>S2) Verify setting the value of WebPA parameter
	 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMUploadLogsNow
	 * to False</li>
	 * <li>S3) Verify retrieving the value of WebPA parameter
	 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMLogsUploadStatus
	 * to check final status</li>
	 * </ol>
	 *
	 * @author Praveen Chandru
	 * 
	 * @refactor yamini.s
	 * 
	 * @param device {@link Dut}
	 * 
	 * 
	 */

	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
	@TestDetails(testUID = "TC-RDKB-LOGUPLOAD-1001")
	public void uploadLogsStatusFalseCondition(Dut device) {
		// String to store the test case ID
		String testId = "TC-RDKB-LOGUPLOAD-101";
		// String to store the test step number
		String stepNumber = null;
		// String to store the error message
		String errorMessage = null;
		// stores the test status
		boolean status = false;

		try {
			// stores the current upload status
			String uploadStatus = null;
			/**
			 * S1 : Verify retrieving the current value of WebPA parameter:
			 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.
			 * xOpsDMLogsUploadStatus
			 */
			stepNumber = "s1";
			status = false;
			LOGGER.info("##################################################################################");
			LOGGER.info(
					"S1: Verify retrieving the current value of WebPA parameter: Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMLogsUploadStatus");
			LOGGER.info("EXPECTED : Should be able to get the value of UploadLogsStatus using WebPA Command");
			LOGGER.info("##################################################################################");
			uploadStatus = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.DEVICE_DEVICE_INFO_RDK_LOG_UPLOAD_STATUS);
			LOGGER.info("The Current Upload Status is " + " " + uploadStatus);

			status = CommonMethods.isNotNull(uploadStatus)
					&& BroadBandUploadLogsUtils.verifyGetParamUploadStatus(device, uploadStatus);
			if (status && !uploadStatus.equalsIgnoreCase(BroadBandTestConstants.STATUS_NOT_TRIGGERED)) {
				uploadStatus = CommonMethods.patternFinder(uploadStatus,
						BroadBandTestConstants.PATTERN_MATCHER_UPLOAD_STATUS);
				LOGGER.info("logStatus is" + " " + uploadStatus);
			}
			errorMessage = "Retrieving the Current value of UploadLog Status failed";
			LOGGER.info("S1 ACTUAL: "
					+ (status ? "Current value of UploadLog Status is retrieved succesfully" : errorMessage));
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, true);

			/**
			 * S2: Verify setting the value of WebPA parameter
			 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.
			 * xOpsDMUploadLogsNow to False
			 */
			stepNumber = "s2";
			status = false;
			LOGGER.info("##################################################################################");
			LOGGER.info(
					"S2: Verify setting the value of WebPA parameter Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMUploadLogsNow to False");
			LOGGER.info(
					"EXPECTED : Should be able to set the value of UploadLogsNow parameter to False using WebPA Command");
			LOGGER.info("##################################################################################");

			status = BroadBandWiFiUtils.setWebPaParams(device,
					BroadBandWebPaConstants.DEVICE_DEVICE_INFO_RDK_LOG_UPLOAD_NOW_STATUS, BroadBandTestConstants.FALSE,
					BroadBandTestConstants.CONSTANT_3);

			errorMessage = "Failed to Set UploadLogsNow Parameter to False";
			LOGGER.info("S2 ACTUAL: " + (status ? "Set Parameter Operation is successful" : errorMessage));
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, true);

			/**
			 * S3 : Verify retrieving the value of WebPA parameter:
			 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.
			 * xOpsDMLogsUploadStatus to check final status
			 */
			stepNumber = "s3";
			status = false;
			LOGGER.info("##################################################################################");
			LOGGER.info(
					"S3: Verify retrieving the value of WebPA parameter: Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMLogsUploadStatus to check final status");
			LOGGER.info(
					"EXPECTED : Get parameter value of UploadLog Status should be same as prior to setting the UploadLogsNow parameter to False");
			LOGGER.info("##################################################################################");
			tapEnv.waitTill(BroadBandTestConstants.FIFTEEN_SECONDS_IN_MILLIS);
			String uploadStatusFalseCondition = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.DEVICE_DEVICE_INFO_RDK_LOG_UPLOAD_STATUS);

			LOGGER.info("uploadStatusFalseCondition is" + " " + uploadStatusFalseCondition);
			if (!uploadStatusFalseCondition.equalsIgnoreCase(BroadBandTestConstants.STATUS_NOT_TRIGGERED)) {
				uploadStatusFalseCondition = CommonMethods.patternFinder(uploadStatusFalseCondition,
						BroadBandTestConstants.PATTERN_MATCHER_UPLOAD_STATUS);
				uploadStatusFalseCondition = CommonMethods.isNotNull(uploadStatusFalseCondition)
						? uploadStatusFalseCondition.trim()
						: null;
				LOGGER.info("logStatus after setting as false is" + " " + uploadStatusFalseCondition);
			}
			status = CommonMethods.isNotNull(uploadStatusFalseCondition) && (uploadStatus.trim()
					.equalsIgnoreCase(uploadStatusFalseCondition.trim())
					|| uploadStatusFalseCondition.trim().equalsIgnoreCase(BroadBandTestConstants.STATUS_NOT_TRIGGERED));
			errorMessage = "Get Parameter value failed to match the expected output";
			LOGGER.info("S3 ACTUAL: " + (status
					? "Upload Status is same before and after setting the UploadLogsNow Parameter to False. No upload happended"
					: errorMessage));
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

		}

		catch (Exception testException) {
			errorMessage = "Exception occured while checking the uploadstatus after setting UploadLogsNow parameter to false: "
					+ testException.getMessage();
			LOGGER.error(errorMessage);
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);
		}

		LOGGER.info("ENDING TEST CASE: TC-RDKB-LOGUPLOAD-1001");

	}

	/**
	 * 
	 * 
	 * Verify when setting UploadlogsNow parameter to true, logs gets uploaded
	 *
	 *
	 * <p>
	 * <ol>
	 * <li>S1) Verify retrieving the current value of WebPA parameter:
	 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMLogsUploadStatus</li>
	 * <li>S2) Verify setting the value of WebPA parameter
	 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMUploadLogsNow
	 * to True</li>
	 * <li>S3) Verify retrieving the value of WebPA parameter:
	 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMLogsUploadStatus
	 * to check the Triggered state</li>
	 * <li>S4) Verify retrieving the value of WebPA parameter:
	 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMLogsUploadStatus
	 * to check the In Progress state</li>
	 * <li>S5) Verify the log upload status in device rdklogs folder and grep the
	 * Log File name</li>
	 * <li>S6) Verify retrieving the value of WebPA parameter
	 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMLogsUploadStatus
	 * and check the final state as Complete</li>
	 * <li>S7) Verifying whether Log file is uploaded in Log server</li>
	 * </ol>
	 *
	 * @author Praveen Chandru
	 * 
	 * @refactor yamini.s
	 * 
	 * @param device {@link Dut}
	 * 
	 * 
	 */

	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
	@TestDetails(testUID = "TC-RDKB-LOGUPLOAD-1002")

	public void uploadLogsStatusCheckUsingWebpa(Dut device) {
		// String to store the test case ID
		String testId = "TC-RDKB-LOGUPLOAD-102";
		// String to store the test step number
		String stepNumber = null;
		// String to store the error message
		String errorMessage = null;
		// stores the test status
		boolean status = false;

		try {
			// stores the previous upload status
			String uploadStatusBeforeLogUpload = null;
			// stores the updated upload status
			String uploadStatusAfterLogUpload = null;
			// initializing the LogFilename
			String uploadedLogFileName = null;
			// stores the current device timestamp
			String currentDeviceTimeStamp = null;

			/**
			 * S1 : Verify retrieving the current value of WebPA parameter:
			 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.
			 * xOpsDMLogsUploadStatus
			 */
			stepNumber = "s1";
			status = false;
			LOGGER.info("##################################################################################");
			LOGGER.info(
					"S1: Verify retrieving the current value of WebPA parameter: Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMLogsUploadStatus");
			LOGGER.info("EXPECTED : Should be able to get the value of UploadLogsStatus using WebPA Command");
			LOGGER.info("##################################################################################");
			currentDeviceTimeStamp = BroadBandCommonUtils.getCurrentTimeStampOnDevice(tapEnv, device);
			uploadStatusBeforeLogUpload = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.DEVICE_DEVICE_INFO_RDK_LOG_UPLOAD_STATUS);
			LOGGER.info("The Current Upload Status is " + " " + uploadStatusBeforeLogUpload);
			status = CommonMethods.isNotNull(uploadStatusBeforeLogUpload)
					&& BroadBandUploadLogsUtils.verifyGetParamUploadStatus(device, uploadStatusBeforeLogUpload);
			errorMessage = "Retrieving the Current value of UploadLog Status failed";
			LOGGER.info("S1 ACTUAL: " + (status ? "Current value of UploadLog Status is retrieved" : errorMessage));
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

			/**
			 * S2: Verify setting the value of WebPA parameter
			 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.
			 * xOpsDMUploadLogsNow to True
			 */
			stepNumber = "s2";
			status = false;
			LOGGER.info("##################################################################################");
			LOGGER.info(
					"S2: Verify setting the value of WebPA parameter Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMUploadLogsNow to True");
			LOGGER.info(
					"EXPECTED : Should be able to set the value of UploadLogsNow parameter to True using WebPA Command");
			LOGGER.info("##################################################################################");

			status = BroadBandWiFiUtils.setWebPaParams(device,
					BroadBandWebPaConstants.DEVICE_DEVICE_INFO_RDK_LOG_UPLOAD_NOW_STATUS, BroadBandTestConstants.TRUE,
					BroadBandTestConstants.CONSTANT_3);
			errorMessage = "Failed to Set UploadLogsNow Parameter to True";
			LOGGER.info("S2 ACTUAL: " + (status ? "Set Parameter Operation is successful" : errorMessage));
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, true);

			/**
			 * S3: Verify retrieving the value of WebPA parameter:
			 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.
			 * xOpsDMLogsUploadStatus to check the Triggered state
			 */
			stepNumber = "s3";
			status = false;
			LOGGER.info("##################################################################################");
			LOGGER.info(
					"S3: Verify retrieving the value of WebPA parameter: Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMLogsUploadStatus to check the Triggered state");
			LOGGER.info("EXPECTED : Value of UploadLogsStatus parameter should move to Triggered State");
			LOGGER.info("##################################################################################");
			status = BroadBandUploadLogsUtils.verifyLogUploadStatusUsingWebPA(device, tapEnv,
					BroadBandTestConstants.STATUS_TRIGGERED);
			errorMessage = "Upload Status did not move to Triggered State";
			LOGGER.info("S3 ACTUAL: " + (status ? "Upload Status moved to Triggered State" : errorMessage));
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

			/**
			 * S4: Verify retrieving the value of WebPA parameter:
			 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.
			 * xOpsDMLogsUploadStatus to check the In Progress state
			 */
			stepNumber = "s4";
			status = false;
			LOGGER.info("##################################################################################");
			LOGGER.info(
					"S4: Verify retrieving the value of WebPA parameter: Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMLogsUploadStatus to check the In Progress state");
			LOGGER.info("EXPECTED : Value of UploadLogsStatus parameter should move to In Progress State");
			LOGGER.info("##################################################################################");

			status = BroadBandUploadLogsUtils.verifyLogUploadStatusUsingWebPA(device, tapEnv,
					BroadBandTestConstants.STATUS_INPROGRESS);
			errorMessage = "Upload Status did not move to In Progress State";
			LOGGER.info("S4 ACTUAL: " + (status ? "Upload Status moved to In Progress State" : errorMessage));
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

			/**
			 * S5:Verify retrieving the value of WebPA parameter
			 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.
			 * xOpsDMLogsUploadStatus and check the successful log uploaded date and time
			 */

			stepNumber = "s5";
			status = false;
			LOGGER.info("##################################################################################");
			LOGGER.info(
					"S5: Verify retrieving the value of WebPA parameter Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMLogsUploadStatus and check the final state as Complete");
			LOGGER.info("EXPECTED : Value of UploadLogsStatus parameter should move to Complete State");
			LOGGER.info("##################################################################################");
			long pollDuration = BroadBandTestConstants.THREE_MINUTES;
			long startTime = System.currentTimeMillis();
			do {
				uploadStatusAfterLogUpload = tapEnv.executeWebPaCommand(device,
						BroadBandWebPaConstants.DEVICE_DEVICE_INFO_RDK_LOG_UPLOAD_STATUS);
				if (!uploadStatusAfterLogUpload.contains(BroadBandTestConstants.STATUS_INPROGRESS)) {
					break;
				}
			} while ((System.currentTimeMillis() - startTime) < pollDuration);
			LOGGER.info("The Previous Upload Status is " + " " + uploadStatusBeforeLogUpload);
			LOGGER.info("The Final Upload Status is" + " " + uploadStatusAfterLogUpload);

			status = BroadBandUploadLogsUtils.uploadStatusCheck(uploadStatusBeforeLogUpload,
					uploadStatusAfterLogUpload);
			errorMessage = "There is no change or the status moved to Failed state after setting the UploadLogsNow parameter to True";
			LOGGER.info("S5 ACTUAL: "
					+ (status ? "Upload Log Time is successfully updated with Complete message" : errorMessage));
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, true);

			/**
			 * S6: Verify the log upload status in device rdklogs folder and grep the Log
			 * file name
			 */
			stepNumber = "s6";
			status = false;
			LOGGER.info("##################################################################################");
			LOGGER.info("S6: Verify the log upload status in device rdklogs folder and grep the Log File name");
			LOGGER.info(
					"EXPECTED : The message LOGS UPLOADED SUCCESSFULLY should be available on ArmConsolelog.txt.0 ");
			LOGGER.info("##################################################################################");
			status = BroadBandUploadLogsUtils.verifyLogUploadMessageInDevice(device, tapEnv, currentDeviceTimeStamp);
			if (status) {
				// Retrieving Log File name to be Uploaded
				uploadedLogFileName = BroadBandUploadLogsUtils.retrieveLogFileName(device, tapEnv);
				LOGGER.info("The Uploaded Log FileName is:" + " " + uploadedLogFileName);
			}

			errorMessage = "LOGS UPLOADED SUCCESSFULLY message not found in the ArmConsolelog.txt.0 file";
			LOGGER.info(
					"S6 ACTUAL: " + (status ? "LOGS UPLOADED SUCCESSFULLY message found in the ArmConsolelog.txt.0 file"
							: errorMessage));
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

			if (BroadbandPropertyFileHandler.isServerConfiguredToUploadToServer()) {

				/**
				 * S7: Verifying whether the log file is uploaded in Log server
				 */

				stepNumber = "s7";
				status = false;
				LOGGER.info("##################################################################################");
				LOGGER.info("S7: Verifying whether Log file is uploaded in Log server ");
				LOGGER.info("EXPECTED : Name of the Log File to be uploaded should be available on Log Server ");
				LOGGER.info("##################################################################################");
				status = BroadBandUploadLogsUtils.verifylogFileNameWithFileNameAction(device, tapEnv,
						uploadedLogFileName);
				errorMessage = "Log File name is not available on the Log Server JSON response. Hence Log File not uploaded";
				LOGGER.info("S7 ACTUAL: "
						+ (status ? "Log File name is available on the Log Server JSON response" : errorMessage));
				tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, true);

			} else {
				LOGGER.info("#######################################################################################");
				LOGGER.info("Step 7 is not applicable if server details to upload logs are not configured");
				LOGGER.info("#######################################################################################");
				tapEnv.updateExecutionForAllStatus(device, testId, "s7", ExecutionStatus.NOT_APPLICABLE,
						"N/A if server details are not configured", false);

			}

		} catch (Exception exception) {
			errorMessage = "Exception occured while verifying the uploadstatus after setting UploadLogsNow parameter to true: "
					+ exception.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, stepNumber, status, errorMessage, true);
		}

		LOGGER.info("ENDING TEST CASE: TC-RDKB-LOGUPLOAD-1002");

	}

	/**
	 * 
	 * 
	 * Verify whether log upload fails after adding dns names in hosts entry and on
	 * removing the dns names log upload should be successful
	 *
	 *
	 * <p>
	 * <ol>
	 * <li>S1) Verify adding the dns names in the host entry file</li>
	 * <li>S2) Verify setting the value of WebPA parameter
	 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMUploadLogsNow
	 * to True</li>
	 * <li>S3) Verify retrieving the value of WebPA parameter:
	 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMLogsUploadStatus
	 * to check the Triggered state</li>
	 * <li>S4) Verify retrieving the value of WebPA parameter:
	 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMLogsUploadStatus
	 * to check the Failed State</li>
	 * <li>S5) Verify removing the dns names in the host entry file</li>
	 * <li>S6) Verify setting the value of WebPA parameter
	 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMUploadLogsNow
	 * to True</li>
	 * <li>S7) Verify retrieving the value of WebPA parameter:
	 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMLogsUploadStatus
	 * to check the Triggered state</li>
	 * <li>S8) Verify retrieving the value of WebPA parameter:
	 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMLogsUploadStatus
	 * to check the In Progress state</li>
	 * <li>S9) Verify retrieving the value of WebPA parameter:
	 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMLogsUploadStatus
	 * to check the Complete state</li>
	 * </ol>
	 *
	 * @author Praveen Chandru
	 * 
	 * @refactor yamini.s
	 * 
	 * @param device {@link Dut}
	 * 
	 * 
	 */

	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
	@TestDetails(testUID = "TC-RDKB-LOGUPLOAD-1003")
	public void uploadLogsNegativeScenarioUsingWebpa(Dut device) {
		// String to store the test case ID
		String testId = "TC-RDKB-LOGUPLOAD-103";
		// String to store the test step number
		String stepNumber = null;
		// String to store the error message
		String errorMessage = null;

		// stores the test status
		boolean status = false;

		try {

			/**
			 * 
			 * S1: Add the following dns names in host entry (/etc/hosts)
			 */
			stepNumber = "s1";
			status = false;
			LOGGER.info("##################################################################################");
			LOGGER.info("S1: Verify adding the dns names in the host entry file");
			LOGGER.info("EXPECTED : DNS names should be added in /etc/hosts");
			LOGGER.info("##################################################################################");
			status = BroadBandUploadLogsUtils.addDnsNamesToHostsFile(device, tapEnv);
			errorMessage = "DNS names are not added in Hosts file";
			LOGGER.info("S1 ACTUAL: " + (status ? "DNS names are added in Hosts file" : errorMessage));
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, true);

			/**
			 * S2: Verify setting the value of WebPA parameter
			 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.
			 * xOpsDMUploadLogsNow to True
			 */
			stepNumber = "s2";
			status = false;
			LOGGER.info("##################################################################################");
			LOGGER.info(
					"S2: Verify setting the value of WebPA parameter Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMUploadLogsNow to True");
			LOGGER.info(
					"EXPECTED : Should be able to set the value of UploadLogsNow parameter to True using WebPA Command");
			LOGGER.info("##################################################################################");

			status = BroadBandWiFiUtils.setWebPaParams(device,
					BroadBandWebPaConstants.DEVICE_DEVICE_INFO_RDK_LOG_UPLOAD_NOW_STATUS, BroadBandTestConstants.TRUE,
					BroadBandTestConstants.CONSTANT_3);
			errorMessage = "Failed to Set UploadLogsNow Parameter to True";
			LOGGER.info("S2 ACTUAL: " + (status ? "Set Parameter Operation is successful" : errorMessage));
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, true);

			/**
			 * S3: Verify retrieving the value of WebPA parameter:
			 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.
			 * xOpsDMLogsUploadStatus to check the Triggered state
			 */
			stepNumber = "s3";
			status = false;
			LOGGER.info("##################################################################################");
			LOGGER.info(
					"S3: Verify retrieving the value of WebPA parameter: Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMLogsUploadStatus to check the Triggered state");
			LOGGER.info("EXPECTED : Value of UploadLogsStatus parameter should move to Triggered State");
			LOGGER.info("##################################################################################");

			status = BroadBandUploadLogsUtils.verifyLogUploadStatusUsingWebPA(device, tapEnv,
					BroadBandTestConstants.STATUS_TRIGGERED);
			errorMessage = "Upload Status did not move to Triggered State";
			LOGGER.info("S3 ACTUAL: " + (status ? "Upload Status moved to Triggered State" : errorMessage));
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

			/**
			 * S4: Verify retrieving the value of WebPA parameter:
			 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.
			 * xOpsDMLogsUploadStatus to check the In Progress state
			 */
			stepNumber = "s4";
			status = false;
			LOGGER.info("##################################################################################");
			LOGGER.info(
					"S4: Verify retrieving the value of WebPA parameter: Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMLogsUploadStatus to check the In Progress state");
			LOGGER.info("EXPECTED : Value of UploadLogsStatus parameter should move to In Progress State");
			LOGGER.info("##################################################################################");

			status = BroadBandUploadLogsUtils.verifyLogUploadStatusUsingWebPA(device, tapEnv,
					BroadBandTestConstants.STATUS_INPROGRESS);
			errorMessage = "Upload Status did not move to In Progress State";
			LOGGER.info("S4 ACTUAL: " + (status ? "Upload Status moved to In Progress State" : errorMessage));
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

			/**
			 * S5: Verify retrieving the value of WebPA parameter:
			 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.
			 * xOpsDMLogsUploadStatus to check the Final state
			 */

			stepNumber = "s5";
			status = false;
			LOGGER.info("##################################################################################");
			LOGGER.info(
					"S5: Verify retrieving the value of WebPA parameter: Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMLogsUploadStatus to check the Failed State");
			LOGGER.info("EXPECTED : Value of UploadLogsStatus parameter should move to Failed State");
			LOGGER.info("##################################################################################");

			status = BroadBandUploadLogsUtils.verifyLogUploadStatusUsingWebPA(device, tapEnv,
					BroadBandTestConstants.STATUS_FAILED);
			errorMessage = "Upload Status did not move to Failed State";
			LOGGER.info("S5 ACTUAL: "
					+ (status ? "Upload Status moved to Failed State. Hence Logupload is blocked" : errorMessage));
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

			/**
			 * S6: Verify removing the dns names in the host entry file
			 * 
			 */

			stepNumber = "s6";
			status = false;
			LOGGER.info("##################################################################################");
			LOGGER.info("S6: Verify removing the dns names in the host entry file");
			LOGGER.info("EXPECTED : DNS names should be removed from /etc/hosts");
			LOGGER.info("##################################################################################");
			status = BroadBandUploadLogsUtils.removeDnsNamesFromHostsFile(device, tapEnv);
			errorMessage = "DNS names are not removed in Hosts file";
			LOGGER.info("S6 ACTUAL: " + (status ? "DNS names are removed in Hosts file" : errorMessage));
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, true);

			/**
			 * S7: Verify setting the value of WebPA parameter
			 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.
			 * xOpsDMUploadLogsNow to True
			 */
			stepNumber = "s7";
			status = false;
			LOGGER.info("##################################################################################");
			LOGGER.info(
					"S7: Verify setting the value of WebPA parameter Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMUploadLogsNow to True");
			LOGGER.info(
					"EXPECTED : Should be able to set the value of UploadLogsNow parameter to True using WebPA Command");
			LOGGER.info("##################################################################################");
			status = BroadBandWiFiUtils.setWebPaParams(device,
					BroadBandWebPaConstants.DEVICE_DEVICE_INFO_RDK_LOG_UPLOAD_NOW_STATUS, BroadBandTestConstants.TRUE,
					BroadBandTestConstants.CONSTANT_3);

			errorMessage = "Failed to Set UploadLogsNow Parameter to True";
			LOGGER.info("S7 ACTUAL: " + (status ? "Set Parameter Operation is successful" : errorMessage));
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, true);

			/**
			 * S8: Verify retrieving the value of WebPA parameter:
			 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.
			 * xOpsDMLogsUploadStatus to check the Triggered state
			 */
			stepNumber = "s8";
			status = false;
			LOGGER.info("##################################################################################");
			LOGGER.info(
					"S8: Verify retrieving the value of WebPA parameter: Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMLogsUploadStatus to check the Triggered state");
			LOGGER.info("EXPECTED : Value of UploadLogsStatus parameter should move to Triggered State");
			LOGGER.info("##################################################################################");

			status = BroadBandUploadLogsUtils.verifyLogUploadStatusUsingWebPA(device, tapEnv,
					BroadBandTestConstants.STATUS_TRIGGERED);
			errorMessage = "Upload Status did not move to Triggered State";
			LOGGER.info("S8 ACTUAL: " + (status ? "Upload Status moved to Triggered State" : errorMessage));
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

			/**
			 * S9: Verify retrieving the value of WebPA parameter:
			 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.
			 * xOpsDMLogsUploadStatus to check the In Progress state
			 */
			stepNumber = "s9";
			status = false;
			LOGGER.info("##################################################################################");
			LOGGER.info(
					"S9: Verify retrieving the value of WebPA parameter: Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMLogsUploadStatus to check the In Progress state");
			LOGGER.info("EXPECTED : Value of UploadLogsStatus parameter should move to In Progress State");
			LOGGER.info("##################################################################################");

			status = BroadBandUploadLogsUtils.verifyLogUploadInProgressStatusUsingWebPA(device, tapEnv,
					BroadBandTestConstants.STATUS_INPROGRESS);
			errorMessage = "Upload Status did not move to In Progress State";
			LOGGER.info("S9 ACTUAL: " + (status ? "Upload Status moved to In Progress State" : errorMessage));
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

			/**
			 * S10: Verify retrieving the value of WebPA parameter:
			 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.
			 * xOpsDMLogsUploadStatus to check the In Progress state
			 */
			stepNumber = "s10";
			status = false;
			LOGGER.info("##################################################################################");
			LOGGER.info(
					"S10: Verify retrieving the value of WebPA parameter: Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMLogsUploadStatus to check the Complete state");
			LOGGER.info("EXPECTED : Value of UploadLogsStatus parameter should move to Complete State");
			LOGGER.info("##################################################################################");

			status = BroadBandUploadLogsUtils.verifyLogUploadStatusUsingWebPA(device, tapEnv,
					BroadBandTestConstants.STATUS_COMPLETE);
			errorMessage = "Upload Status did not move to Complete State";
			LOGGER.info("S10 ACTUAL: " + (status ? "Upload Status moved to Complete State" : errorMessage));
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, true);

		} catch (Exception exception) {
			errorMessage = "Exception occured while verifying the uploadstatus after adding the dns names for blocking the log upload: "
					+ exception.getMessage();
			LOGGER.error(errorMessage);
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-LOGUPLOAD-1003");

	}

	/**
	 * 
	 * 
	 * Verify when setting UploadlogsNow snmp OID (1.3.6.1.4.1.17270.43.1.2.1.0) to
	 * False using snmp, no upload happens
	 *
	 *
	 * <p>
	 * <ol>
	 * <li>S1) Verify retrieving the value of
	 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMLogsUploadStatus
	 * using SNMP OID (1.3.6.1.4.1.17270.43.1.2.2.0)</li>
	 * <li>S2) Verify setting the value of
	 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMUploadLogsNow
	 * to False using SNMP OID *(1.3.6.1.4.1.17270.43.1.2.1.0)</li>
	 * <li>S3) Verify retrieving the value of
	 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMLogsUploadStatus
	 * using SNMP OID (1.3.6.1.4.1.17270.43.1.2.2.0) to check the final upload
	 * status</li>
	 * </ol>
	 *
	 * @author Praveen Chandru,Joseph Maduram
	 * 
	 * @refactor yamini.s
	 * 
	 * @param device {@link Dut}
	 * 
	 * 
	 */
	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
	@TestDetails(testUID = "TC-RDKB-LOGUPLOAD-1004")

	public void uploadLogsFalseConditionSnmp(Dut device) {
		// String to store the test case ID
		String testId = "TC-RDKB-LOGUPLOAD-104";
		// String to store the test step number
		String stepNumber = null;
		// String to store the error message
		String errorMessage = null;
		// stores the test status
		boolean status = false;

		try {
			// Stores the Snmpsetresponse
			String snmpSetResponse = null;
			/**
			 * S1: Verify retrieving the current value of
			 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.
			 * xOpsDMLogsUploadStatus using SNMP OID (1.3.6.1.4.1.17270.43.1.2.2.0)
			 */
			stepNumber = "s1";
			status = false;
			LOGGER.info("##################################################################################");
			LOGGER.info(
					"S1: Verify retrieving the value of Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMLogsUploadStatus using SNMP OID (1.3.6.1.4.1.17270.43.1.2.2.0)");
			LOGGER.info("EXPECTED : Should be able to get the value of UploadLogsStatus using Snmp Command");
			LOGGER.info("##################################################################################");
			String uploadStatusSnmp = BroadBandSnmpUtils.executeSnmpGetOnRdkDevices(tapEnv, device,
					BroadBandSnmpMib.ECM_UPLOAD_STATUS.getOid());
			status = CommonMethods.isNotNull(uploadStatusSnmp)
					&& BroadBandUploadLogsUtils.verifyGetParamUploadStatus(device, uploadStatusSnmp);
			if (status && !uploadStatusSnmp.equalsIgnoreCase(BroadBandTestConstants.STATUS_NOT_TRIGGERED)) {
				uploadStatusSnmp = CommonMethods.patternFinder(uploadStatusSnmp,
						BroadBandTestConstants.PATTERN_MATCHER_UPLOAD_STATUS);
				LOGGER.info("logStatus is" + " " + uploadStatusSnmp);
			}
			errorMessage = "Retrieving the Current value of UploadLog Status failed";
			LOGGER.info("S1 ACTUAL: "
					+ (status ? "Current value of UploadLog Status is retrieved succesfully" : errorMessage));
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, ErrorType.SNMP + errorMessage, false);

			/**
			 * S2: Verify setting the value of
			 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.
			 * xOpsDMUploadLogsNow to False using SNMP OID *(1.3.6.1.4.1.17270.43.1.2.1.0)
			 */

			stepNumber = "s2";
			status = false;
			LOGGER.info("##################################################################################");
			LOGGER.info(
					"S2: Verify setting the value of Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMUploadLogsNow to False using SNMP OID *(1.3.6.1.4.1.17270.43.1.2.1.0)");
			LOGGER.info(
					"EXPECTED : Should be able to set the value of UploadLogsNow parameter to False using snmp Command");
			LOGGER.info("##################################################################################");
			snmpSetResponse = BroadBandSnmpUtils.retrieveSnmpSetOutputWithDefaultIndexOnRdkDevices(device, tapEnv,
					BroadBandSnmpMib.ECM_UPLOAD_LOG_NOW.getOid(), SnmpDataType.INTEGER,
					BroadBandTestConstants.STRING_VALUE_TWO);
			status = CommonMethods.isNotNull(snmpSetResponse)
					&& snmpSetResponse.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_TWO);
			errorMessage = "Failed to set the value to false using the SNMP OID";
			LOGGER.info("S2 ACTUAL: " + (status ? "Snmpset operation is successful" : errorMessage));
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, ErrorType.SNMP + errorMessage, true);

			/**
			 * S3: Verify retrieving the value of
			 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.
			 * xOpsDMLogsUploadStatus using SNMP OID (1.3.6.1.4.1.17270.43.1.2.2.0) to check
			 * the final upload status
			 */
			stepNumber = "s3";
			status = false;
			LOGGER.info("##################################################################################");
			LOGGER.info(
					"S3: Verify retrieving the value of Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMLogsUploadStatus using SNMP OID (1.3.6.1.4.1.17270.43.1.2.2.0) to check the final upload status");
			LOGGER.info(
					"EXPECTED : SnmpGet value of UploadLog Status should be same as prior to setting the UploadLogsNow parameter to False");
			LOGGER.info("##################################################################################");
			String uploadStatusFalseConditionSnmp = BroadBandSnmpUtils.executeSnmpGetOnRdkDevices(tapEnv, device,
					BroadBandSnmpMib.ECM_UPLOAD_STATUS.getOid());
			LOGGER.info("The Previous Upload Status is" + " " + uploadStatusSnmp.trim());

			if (!uploadStatusFalseConditionSnmp.equalsIgnoreCase(BroadBandTestConstants.STATUS_NOT_TRIGGERED)) {
				uploadStatusFalseConditionSnmp = CommonMethods.patternFinder(uploadStatusFalseConditionSnmp,
						BroadBandTestConstants.PATTERN_MATCHER_UPLOAD_STATUS);
				uploadStatusFalseConditionSnmp = CommonMethods.isNotNull(uploadStatusFalseConditionSnmp)
						? uploadStatusFalseConditionSnmp.trim()
						: null;

			}
			LOGGER.info("The Final Upload Status is" + " " + uploadStatusFalseConditionSnmp.trim());
			status = CommonMethods.isNotNull(uploadStatusFalseConditionSnmp)
					&& (uploadStatusSnmp.trim().equalsIgnoreCase(uploadStatusFalseConditionSnmp.trim())
							|| uploadStatusFalseConditionSnmp.trim()
									.equalsIgnoreCase(BroadBandTestConstants.STATUS_NOT_TRIGGERED));
			errorMessage = "Upload Status is not same before and after setting the SNMP OID to False";
			LOGGER.info("S3 ACTUAL: " + (status
					? "Upload Status is same before and after setting the SNMP OID to False. No upload happended"
					: errorMessage));
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, ErrorType.SNMP + errorMessage, false);

		} catch (Exception exception) {
			errorMessage = "Exception occured while verifying the uploadstatus after setting UploadLogsNow Snmp OID to false: "
					+ exception.getMessage();
			LOGGER.error(errorMessage);
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);
		}

		LOGGER.info("ENDING TEST CASE: TC-RDKB-LOGUPLOAD-1004");

	}

	/**
	 * 
	 * Verify when setting UploadlogsNow snmp OID (1.3.6.1.4.1.17270.43.1.2.1.0) to
	 * True, logs get uploaded
	 *
	 * <p>
	 * <ol>
	 * <li>S1) Verify retrieving the value of
	 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMLogsUploadStatus
	 * using SNMP OID (1.3.6.1.4.1.17270.43.1.2.2.0)</li>
	 * <li>S2) Verify setting the value of
	 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMUploadLogsNow
	 * to True using SNMP OID (1.3.6.1.4.1.17270.43.1.2.1.0)</li>
	 * <li>S3) Verify retrieving the value of
	 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMLogsUploadStatus
	 * using SNMP OID (1.3.6.1.4.1.17270.43.1.2.2.0) to check Triggered /In Progress
	 * State</li>
	 * <li>S4) Verify the log upload status in device rdklogs folder and grep the
	 * Log File name</li>
	 * <li>S5) Verify retrieving the value of
	 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMLogsUploadStatus
	 * using SNMP OID (1.3.6.1.4.1.17270.43.1.2.2.0) to check the final upload
	 * status as Complete</li>
	 * <li>S6) Verifying whether Log file is uploaded in Log server</li>
	 * </ol>
	 *
	 * @author Praveen Chandru
	 * 
	 * @refactor yamini.s
	 * 
	 * @param device {@link Dut}
	 */
	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
	@TestDetails(testUID = "TC-RDKB-LOGUPLOAD-1005")
	public void uploadLogsStatusCheckUsingSnmp(Dut device) {

		// Variable Declaration begins
		// String to store the test case ID
		String testId = "TC-RDKB-LOGUPLOAD-105";
		// String to store the test step number
		String stepNumber = null;
		// String to store the error message
		String errorMessage = null;
		// stores the test status
		boolean status = false;
		// Stores the snmp set response
		String snmpSetResponse = null;
		// Stores the previous Upload status
		String initialLogUploadStatus = null;
		// Stores the latest Upload status
		String finalLogUploadStatus = null;
		// initialize the log file name
		String uploadedLogFileName = null;
		// stores the current device time stamp
		String currentDeviceTimeStamp = null;
		// Variable to store response
		String response = null;
		// Variable to store response
		String command = null;

		// Variable Declaration Ends

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-LOGUPLOAD-1005");
		LOGGER.info(
				"TEST DESCRIPTION: Verify when setting UploadlogsNow parameter to True using snmp, logs get uploaded");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info(
				"1.Verify retrieving the value of Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMLogsUploadStatus"
						+ " using SNMP OID (1.3.6.1.4.1.17270.43.1.2.2.0)");
		LOGGER.info(
				"2. Verify setting the value of Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMUploadLogsNow "
						+ "to True using SNMP OID (1.3.6.1.4.1.17270.43.1.2.1.0)");
		LOGGER.info(
				"3.Verify retrieving the value of Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMLogsUploadStatus "
						+ "using SNMP OID (1.3.6.1.4.1.17270.43.1.2.2.0) to check Triggered /In Progress State");
		LOGGER.info(
				"4. Verify retrieving the value of Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMLogsUploadStatus "
						+ "using SNMP OID  (1.3.6.1.4.1.17270.43.1.2.2.0) to check the final upload status as Complete");
		LOGGER.info("5. Verify the log upload status in device rdklogs folder and grep the Log File name");
		LOGGER.info("6. Verifying whether Log file is uploaded in Log server");

		LOGGER.info("#######################################################################################");
		try {
			stepNumber = "s1";
			status = false;
			errorMessage = "Retrieving the Current value of UploadLog Status via SNMP failed";
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 1: DESCRIPTION :   Verify retrieving the value of Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMLogsUploadStatus"
							+ " using SNMP OID (1.3.6.1.4.1.17270.43.1.2.2.0)");
			LOGGER.info("STEP 1: ACTION : Execute 1.3.6.1.4.1.17270.43.1.2.2.0 oid via snmp and verify response");
			LOGGER.info("STEP 1: EXPECTED : Should be able to get the value of UploadLogsStatus using Snmp Command");
			LOGGER.info("**********************************************************************************");
			currentDeviceTimeStamp = BroadBandCommonUtils.getCurrentTimeStampOnDevice(tapEnv, device);
			initialLogUploadStatus = BroadBandSnmpUtils.executeSnmpGetOnRdkDevices(tapEnv, device,
					BroadBandSnmpMib.ECM_UPLOAD_STATUS.getOid());
			LOGGER.info("The Current Upload Status is : " + initialLogUploadStatus);
			status = CommonMethods.isNotNull(initialLogUploadStatus)
					&& BroadBandUploadLogsUtils.verifyGetParamUploadStatus(device, initialLogUploadStatus);
			if (status) {
				LOGGER.info(
						"STEP 1: ACTUAL :SUCCESSFULLY OBTAINED LOG UPLOAD STATUS USING 1.3.6.1.4.1.17270.43.1.2.2.0 SNMP OID");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, ErrorType.SNMP + errorMessage, false);

			stepNumber = "s2";
			status = false;
			errorMessage = "Snmpset failed to set the value to True using the OID";
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 2: DESCRIPTION :   Verify setting the value of Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMUploadLogsNow"
							+ " to True using SNMP OID (1.3.6.1.4.1.17270.43.1.2.1.0)");
			LOGGER.info("STEP 2: ACTION : Set 1.3.6.1.4.1.17270.43.1.2.1.0 oid value as 1 using snmp");
			LOGGER.info(
					"STEP 2: EXPECTED : Should be able to set the value of UploadLogsNow parameter to True using Snmp Command");
			LOGGER.info("**********************************************************************************");
			try {
				if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
					response = tapEnv.executeCommandUsingSsh(device,
							BroadBandCommandConstants.CMD_GET_LOGUPLOAD_ARMCONSOLE_LOG);
				} else {
					if (!DeviceModeHandler.isRPIDevice(device)) {
						response = tapEnv.executeCommandUsingSsh(device,
								BroadBandCommandConstants.CMD_GET_LOGUPLOAD_CONSOLE_LOG);
					} else {
						response = tapEnv.executeCommandUsingSsh(device,
								"su -c " + BroadBandTestConstants.DOUBLE_QUOTE
										+ BroadBandCommandConstants.CMD_GET_LOGUPLOAD_CONSOLE_LOG
										+ BroadBandTestConstants.DOUBLE_QUOTE);

					}

				}

			} catch (Exception exception) {
				LOGGER.error("Exception occured while searching log ", exception);

			}
			tapEnv.waitTill(BroadBandTestConstants.FIFTY_SECONDS_IN_MILLIS);
			snmpSetResponse = BroadBandSnmpUtils.retrieveSnmpSetOutputWithDefaultIndexOnRdkDevices(device, tapEnv,
					BroadBandSnmpMib.ECM_UPLOAD_LOG_NOW.getOid(), SnmpDataType.INTEGER,
					BroadBandTestConstants.STRING_VALUE_ONE);

			status = CommonMethods.isNotNull(snmpSetResponse)
					&& snmpSetResponse.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_ONE);
			ArrayList<String> logStatusList = new ArrayList<>();
			if (status) {
				logStatusList = BroadBandUploadLogsUtils.getLogUploadStatusUsingSnmp(device, tapEnv,
						BroadBandTestConstants.EIGHT_MINUTE_IN_MILLIS);
			}
			if (status) {
				LOGGER.info(
						"STEP 2: ACTUAL :SUCCESSFULLY TRIGGERED LOG UPLOAD USING 1.3.6.1.4.1.17270.43.1.2.1.0 SNMP OID");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, ErrorType.SNMP + errorMessage, true);

			stepNumber = "s3";
			status = false;
			boolean completedStatus = false;
			errorMessage = "Upload Status did not move to Triggered /In Progress State";
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 3: DESCRIPTION : Verify retrieving the value of Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMLogsUploadStatus"
							+ " using SNMP OID (1.3.6.1.4.1.17270.43.1.2.2.0) to check Triggered /In Progress State");
			LOGGER.info("STEP 3: ACTION : Execute 1.3.6.1.4.1.17270.43.1.2.2.0 oid via snmp and verify response");
			LOGGER.info(
					"STEP 3: EXPECTED : Value of UploadLogsStatus Snmp OID should move to Triggered /In Progress State");
			LOGGER.info("**********************************************************************************");
			LOGGER.info("Obtained Array list size : " + logStatusList.size());
			for (int iteration = 0; iteration < logStatusList.size(); iteration++) {

				LOGGER.info("Obtained CDL status " + iteration + 1 + ": " + logStatusList.get(iteration));
			}
			if (logStatusList.contains(BroadBandTestConstants.STATUS_TRIGGERED)
					|| logStatusList.contains(BroadBandTestConstants.STATUS_INPROGRESS)) {
				status = true;

			} else if (logStatusList.contains(BroadBandTestConstants.STATUS_COMPLETE)) {

				finalLogUploadStatus = BroadBandSnmpUtils.executeSnmpGetOnRdkDevices(tapEnv, device,
						BroadBandSnmpMib.ECM_UPLOAD_STATUS.getOid());
				status = BroadBandUploadLogsUtils.uploadStatusCheck(initialLogUploadStatus, finalLogUploadStatus);
				if (status) {
					completedStatus = status;
				}
			} else if (logStatusList.contains(BroadBandTestConstants.STATUS_FAILED)
					|| logStatusList.contains(BroadBandTestConstants.STATUS_FAILURE)) {

				errorMessage = "Log upload trigger is failed when we initiate log upload via SNMP MIB !!!";

			} else if (logStatusList.contains(BroadBandTestConstants.STATUS_NOT_TRIGGERED)) {

				errorMessage = "Log upload is not triggered even after 5 minutes of log upload initiate using SNMP MIB !!!";
			}
			if (status) {
				LOGGER.info(
						"STEP 3: SUCCESSFULLY VERIFIED LOG UPLOAD STATUS AS TRIGGERED/IN PROGRESS USING 1.3.6.1.4.1.17270.43.1.2.2.0 SNMP OID");
			} else {
				LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, ErrorType.SNMP + errorMessage, true);

			stepNumber = "s4";
			status = false;
			errorMessage = "There is no change or the status moved to Failed state after setting the UploadLogsNow parameter to True using SNMP OID";
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 4: DESCRIPTION : Verify retrieving the value of Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMLogsUploadStatus"
							+ " using SNMP OID (1.3.6.1.4.1.17270.43.1.2.2.0) to check the final upload status as Complete");
			LOGGER.info("STEP 4: ACTION : Execute 1.3.6.1.4.1.17270.43.1.2.2.0 oid via snmp and verify response");
			LOGGER.info("STEP 4: EXPECTED : Value of UploadLogsStatus Snmp OID should move to Complete State");
			LOGGER.info("**********************************************************************************");
			if (logStatusList.contains(BroadBandTestConstants.STATUS_COMPLETE)) {
				status = completedStatus ? completedStatus : true;
			}
			if (status) {
				LOGGER.info(
						"STEP 4: SUCCESSFULLY VERIFIED LOG UPLOAD STATUS AS COMPLETED USING 1.3.6.1.4.1.17270.43.1.2.2.0 SNMP OID");
			} else {
				LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, ErrorType.SNMP + errorMessage, true);

			stepNumber = "s5";
			status = false;
			errorMessage = "LOGS UPLOADED SUCCESSFULLY message not found in the ArmConsolelog.txt.0 file";
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 5: DESCRIPTION : Verify the log upload status in device rdklogs folder and grep the Log File name");
			LOGGER.info(
					"STEP 5: ACTION : Execute grep -i \"LOGS UPLOADED SUCCESSFULLY\" /rdkblogs/logs/ArmConsolelog.txt.0");
			LOGGER.info(
					"STEP 5: EXPECTED : The message LOGS UPLOADED SUCCESSFULLY should be available on ArmConsolelog.txt.0");
			LOGGER.info("**********************************************************************************");
			try {
				command = BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.GREP_COMMAND,
						BroadBandTraceConstants.LOGS_UPLOADED_SUCCESSFULLY,
						BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
						BroadBandCommandConstants.FILE_PATH_TMP_CONSOLE_TAIL, BroadBandTestConstants.SYMBOL_PIPE,
						BroadBandTestConstants.CMD_TAIL_1);
				response = tapEnv.executeCommandUsingSsh(device, command);
				LOGGER.info("RESPONSE-----" + response);
				status = CommonMethods.isNotNull(response)
						&& response.contains(BroadBandTraceConstants.LOG_UPLOAD_STATUS);
				CommonUtils.removeFileandVerifyFileRemoval(tapEnv, device,
						BroadBandCommandConstants.FILE_PATH_TMP_CONSOLE_TAIL);
			} catch (Exception exception) {
				LOGGER.error("Exception occured while searching log ", exception);

			}
			if (status) {
				errorMessage = "Unable to retreive the Lo file name, After successful log upload.";
				// Retrieving Log File name to be Uploaded
				uploadedLogFileName = BroadBandUploadLogsUtils.retrieveLogFileName(device, tapEnv);
				LOGGER.info("The Uploaded Log FileName is : " + uploadedLogFileName);
			}
			if (status) {
				LOGGER.info("STEP 5: LOGS UPLOADED SUCCESSFULLY message found in the ArmConsolelog.txt.0 file");
			} else {
				LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

			if (BroadbandPropertyFileHandler.isServerConfiguredToUploadToServer()) {

				tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTES_IN_SECONDS);
				stepNumber = "s6";
				status = false;
				errorMessage = "curl response 'HTTP/1.1 200 is not available on the Log Server response. Hence Log File not uploaded";
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP 6: DESCRIPTION : Verifying whether Log file is uploaded in prod log server");
				LOGGER.info("STEP 6: ACTION : Execute curl  command and verify response");
				LOGGER.info(
						"STEP 6: EXPECTED : Log file should be available with curl response of S3 path of the filename ");
				LOGGER.info("**********************************************************************************");
				status = BroadBandUploadLogsUtils.verifylogFileNameWithFileNameAction(device, tapEnv,
						uploadedLogFileName);
				if (status) {
					LOGGER.info("STEP 6: SUCCESSFULLY VERIFIED UPLOADED LOG FILE AVAILABILITY IN PROD LOG SERVER ");
				} else {
					LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, true);

			} else {
				LOGGER.info("#######################################################################################");
				LOGGER.info("Step 6 is not applicable if server details to upload logs are not configured");
				LOGGER.info("#######################################################################################");
				tapEnv.updateExecutionForAllStatus(device, testId, "s6", ExecutionStatus.NOT_APPLICABLE,
						"N/A if server details are not configured", false);

			}
		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, stepNumber, status, errorMessage,
					false);
		}

		LOGGER.info("ENDING TEST CASE: TC-RDKB-LOGUPLOAD-1005");

	}

	/**
	 *
	 * Test Case : Verify successful log upload post upgrade and reboot
	 *
	 * <p>
	 * STEPS:
	 * </p>
	 * <ol>
	 * <li>Pre Condition 1 : Get the current firmware version on the device.</li>
	 * <li>Step 1 : Verify triggering CDL with Latest Firmware version(Latest GA
	 * Build) via TR-181/SNMP.</li>
	 * <li>Step 2 : Verify checking the log file name with latest time and file
	 * upload successful (HTTP 200) from Consolelog.txt.0/Armconsolelog.txt.0,
	 * within 30 mins of uptime.</li>
	 * <li>Step 3 : Verify Rebooting the device and verify whether device comes up
	 * properly with all required process</li>
	 * <li>Step 4 : Verify checking the log file name with latest time and file
	 * upload successful (HTTP 200) from Consolelog.txt.0/Armconsolelog.txt.0,
	 * within 30 mins of uptime.</li>
	 * <li>Step 5 : Verify triggered CDL successfull with intial firmware
	 * version(Test Build)</li>
	 * <li>Step 6 : Verify checking the log file name with latest time and file
	 * upload successful (HTTP 200) from Consolelog.txt.0/Armconsolelog.txt.0,
	 * within 30 mins of uptime.</li>
	 * <li>Step 7 : Verify Rebooting the device and verify whether device comes up
	 * properly with all required process</li>
	 * <li>Step 8 : Verify checking the log file name with latest time and file
	 * upload successful (HTTP 200) from Consolelog.txt.0/Armconsolelog.txt.0,
	 * within 30 mins of uptime.</li>
	 * <li>Post Condition 1 : Perform flashing the original build on the
	 * device.</li>
	 * </ol>
	 * 
	 * @param device {@link Instance of Dut}
	 * 
	 * @author Vignesh
	 * @refactor Said Hisham
	 */
	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = BroadBandTestGroup.SYSTEM)
	@TestDetails(testUID = "TC-RDKB-UPGRADE-LOGUPLOAD-6001")
	public void testToVerifyLogUploadPostRebootAndUpgrade(Dut device) {

		// Variable Declaration begins
		String testCaseId = "TC-RDKB-UPGRADE-LOGUPLOAD-601";
		String stepNum = "S1";
		String errorMessage = null;
		boolean status = false;
		String initialFirmwareVersion = null;
		boolean hasLatestBuildChanged = false;
		String imageNameForCdl = null;
		boolean hasOriginalBuildChanged = false;
		// Variable Declaration Ends
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-UPGRADE-LOGUPLOAD-6001");
		LOGGER.info("TEST DESCRIPTION: Verify successful log upload post upgrade and reboot");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info("Pre Condition 1 : Get the current firmware version on the device.");
		LOGGER.info("1. Verify triggering CDL with Latest Firmware version(Latest GA Build) via TR-181/SNMP.");
		LOGGER.info(
				"2. Verify checking the log file name with latest time and file upload successful (HTTP 200) from Consolelog.txt.0/Armconsolelog.txt.0, within 30 mins of uptime.");
		LOGGER.info("3. Reboot the device and verify whether device comes up properly with all required process");
		LOGGER.info(
				"4. Verify checking the log file name with latest time and file upload successful (HTTP 200) from Consolelog.txt.0/Armconsolelog.txt.0, within 30 mins of uptime.");
		LOGGER.info("5. Verify triggered CDL successfull with intial firmware version(Test Build)");
		LOGGER.info(
				"6. Verify checking the log file name with latest time and file upload successful (HTTP 200) from Consolelog.txt.0/Armconsolelog.txt.0, within 30 mins of uptime.");
		LOGGER.info("7. Reboot the device and verify whether device comes up properly with all required process");
		LOGGER.info(
				"8. Verify checking the log file name with latest time and file upload successful (HTTP 200) from Consolelog.txt.0/Armconsolelog.txt.0, within 30 mins of uptime.");
		LOGGER.info("Post Condition 1 : Perform flashing the original build on the device.");
		LOGGER.info("#######################################################################################");
		try {
			initialFirmwareVersion = BroadBandPreConditionUtils.executePreConditionToGetCurrentFirmwareVersion(device,
					tapEnv, BroadBandTestConstants.CONSTANT_1);
			LOGGER.info(
					"Current firmware version/Initial Firmware Version in the device is :" + initialFirmwareVersion);

			errorMessage = null;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 1: DESCRIPTION : Verify triggering CDL with Latest Firmware version(Latest GA Build) via TR-181/SNMP.");
			LOGGER.info("STEP 1: ACTION : Trigger the CDL with latest firmware version using TR-181/SNMP");
			LOGGER.info("STEP 1: EXPECTED : CDL must be successful with latest firmware version.");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to get the current Firmware version from the gateway";
			try {
				if (CommonMethods.isNotNull(initialFirmwareVersion)) {
					errorMessage = "Unable to get the Latest GA Firmware version for the gateway";

					imageNameForCdl = tapEnv.getLatestBuildImageVersionForCdlTrigger(device, true);
					LOGGER.info("Image Name For Cdl:" + imageNameForCdl);

					if (CommonMethods.isNull(imageNameForCdl)) {
						LOGGER.info(
								" GA image obtained from deployed version service is null. Hence getting the image from property file ");
						imageNameForCdl = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(
								device, BroadBandPropertyKeyConstants.PARTIAL_PROPERTY_KEY_FOR_GA_IMAGE);
						LOGGER.info("Latest Firmware version from property file: " + imageNameForCdl);
					}

					if (CommonMethods.isNotNull(imageNameForCdl)) {
						if (!BroadBandCommonUtils.compareValues(BroadBandTestConstants.CONSTANT_TXT_COMPARISON,
								initialFirmwareVersion, imageNameForCdl)) {
							errorMessage = "Unable to trriger the image upgrade on the device with latest firmware version(Latest GA Build).";
							hasLatestBuildChanged = BroadBandCodeDownloadUtils
									.triggerUpgradeOrDowngradeGivenBuild(device, tapEnv, imageNameForCdl, false);
							status = hasLatestBuildChanged;
							LOGGER.info("Flashed the latest build on the device : " + status);
						}
					}
				}
			} catch (Exception e) {
				errorMessage += e.getMessage();
			}
			if (status) {
				LOGGER.info("STEP 1: ACTUAL : Successfully upgraded to latest GA build version");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			stepNum = "S2";
			errorMessage = "Unable to verify the latest log file name or Upload is not successful with HTTP 200";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 2: DESCRIPTION : Verify checking the log file name with latest time and file upload successful (HTTP 200) from Consolelog.txt.0/Armconsolelog.txt.0, within 30 mins of uptime.");
			LOGGER.info("STEP 2: ACTION : Execute Command : cat Consolelog.txt.0 ");
			LOGGER.info(
					"STEP 2: EXPECTED : Log file name should be latest with time greater than uptime after upgrade and file upload successful (HTTP 200) from Consolelog.txt.0/Armconsolelog.txt.0, within 30 mins of uptime.");
			LOGGER.info("**********************************************************************************");
			status = BroadBandCommonUtils.verifyLatestLogUploadSuccessful(tapEnv, device);
			if (status) {
				LOGGER.info(
						"STEP 2: ACTUAL : Log file name is latest with time greater than uptime after upgrade and file upload is successful with (HTTP 200) from Consolelog.txt.0/Armconsolelog.txt.0, within 30 mins of uptime");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S3";
			errorMessage = "Unable to access STB after reboot!!!!";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 3: DESCRIPTION : Reboot the device and verify whether device comes up properly with all required process");
			LOGGER.info("STEP 3: ACTION : Reboot the device");
			LOGGER.info("STEP 3: EXPECTED : Box should reboot and it should be accessable after reboot");
			LOGGER.info("**********************************************************************************");
			status = CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv);
			if (status) {
				LOGGER.info(
						"STEP 3: ACTUAL : Successfully rebooted device and device has come up with all required process");
			} else {
				LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			stepNum = "S4";
			errorMessage = "Unable to verify the latest log file name or Upload is not successful with HTTP 200";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 4: DESCRIPTION : Verify checking the log file name with latest time and file upload successful (HTTP 200) from Consolelog.txt.0/Armconsolelog.txt.0, within 30 mins of uptime.");
			LOGGER.info("STEP 4: ACTION : Execute Command : cat Consolelog.txt.0");
			LOGGER.info(
					"STEP 4: EXPECTED : Log file name should be latest with time greater than uptime after reboot and file upload successful (HTTP 200) from Consolelog.txt.0/Armconsolelog.txt.0, within 30 mins of uptime.");
			LOGGER.info("**********************************************************************************");
			status = BroadBandCommonUtils.verifyLatestLogUploadSuccessful(tapEnv, device);
			if (status) {
				LOGGER.info(
						"STEP 4: ACTUAL : Log file name is latest with time greater than uptime after reboot and file upload is also successful (HTTP 200) from Consolelog.txt.0/Armconsolelog.txt.0, within 30 mins of uptime");
			} else {
				LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S5";
			errorMessage = "Unable to trigger the image upgrade on the device with initial firmware version";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 5: DESCRIPTION : Verify triggered CDL successfull with intial firmware version(Test Build)");
			LOGGER.info("STEP 5: ACTION : Trigger the cdl with initial firmware version using TR-181/SNMP");
			LOGGER.info("STEP 5: EXPECTED : Cdl successful with initial firmware version");
			LOGGER.info("**********************************************************************************");
			try {
				hasOriginalBuildChanged = BroadBandCodeDownloadUtils.triggerUpgradeOrDowngradeGivenBuild(device, tapEnv,
						initialFirmwareVersion, false);
				status = hasOriginalBuildChanged;
				LOGGER.info("Flashed the original build on the device: " + status);
			} catch (Exception e) {
				errorMessage += e.getMessage();
			}
			if (status) {
				LOGGER.info("STEP 5: ACTUAL : Cdl successful with initial firmware version.");
			} else {
				LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			stepNum = "S6";
			errorMessage = "Unable to verify the latest log file name or Upload is not successful with HTTP 200";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 6: DESCRIPTION : Verify checking the log file name with latest time and file upload successful (HTTP 200) from Consolelog.txt.0/Armconsolelog.txt.0, within 30 mins of uptime.");
			LOGGER.info("STEP 6: ACTION : Execute Command : cat Consolelog.txt.0");
			LOGGER.info(
					"STEP 6: EXPECTED : Log file name should be latest with time greater than uptime after upgrade and file upload successful (HTTP 200) from Consolelog.txt.0/Armconsolelog.txt.0, within 30 mins of uptime.");
			LOGGER.info("**********************************************************************************");
			status = BroadBandCommonUtils.verifyLatestLogUploadSuccessful(tapEnv, device);
			if (status) {
				LOGGER.info(
						"STEP 6: ACTUAL : Log file name is latest with time greater than uptime after upgrade and file upload is successful with (HTTP 200) from Consolelog.txt.0/Armconsolelog.txt.0, within 30 mins of uptime");
			} else {
				LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S7";
			errorMessage = "Unable to access STB after reboot!!!!";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 7: DESCRIPTION : Reboot the device and verify whether device comes up properly with all required process");
			LOGGER.info("STEP 7: ACTION : Reboot the device");
			LOGGER.info("STEP 7: EXPECTED : Box should reboot and it should be accessable after reboot");
			LOGGER.info("**********************************************************************************");
			status = CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv);
			if (status) {
				LOGGER.info(
						"STEP 7: ACTUAL : Successfully rebooted device and device has come up with all required process");
			} else {
				LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			stepNum = "S8";
			errorMessage = "Unable to verify the lastest log file name or Upload is not successful with HTTP 200";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 8: DESCRIPTION : Verify checking the log file name with latest time and file upload successful (HTTP 200) from Consolelog.txt.0/Armconsolelog.txt.0, within 30 mins of uptime.");
			LOGGER.info("STEP 8: ACTION : Execute Command : cat Consolelog.txt.0");
			LOGGER.info(
					"STEP 8: EXPECTED : Log file name should be latest with time greater than uptime after reboot and file upload successful (HTTP 200) from Consolelog.txt.0/Armconsolelog.txt.0, within 30 mins of uptime.");
			LOGGER.info("**********************************************************************************");
			status = BroadBandCommonUtils.verifyLatestLogUploadSuccessful(tapEnv, device);
			if (status) {
				LOGGER.info(
						"STEP 8: ACTUAL :  Log file name is latest with time greater than uptime after reboot and file upload is also successful (HTTP 200) from Consolelog.txt.0/Armconsolelog.txt.0, within 30 mins of uptime");
			} else {
				LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			/**
			 * POST-CONDITION 1 : PERFORM FLASHING THE ORIGINAL BUILD ON THE DEVICE
			 */
			BroadBandPostConditionUtils.executePostConditionToTriggerCdl(device, tapEnv, hasLatestBuildChanged,
					hasOriginalBuildChanged, BroadBandTestConstants.CONSTANT_1, initialFirmwareVersion);
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-UPGRADE-LOGUPLOAD-6001");

	}

	/**
	 * 
	 * 
	 * Verify whether log upload fails after adding dns names in hosts entry and on
	 * removing the dns names log upload should be successful
	 *
	 *
	 * <p>
	 * <ol>
	 * <li>S1) Verify adding the dns names in the host entry file</li>
	 * <li>S2) Verify setting the value of
	 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMUploadLogsNow
	 * to True using SNMP OID (1.3.6.1.4.1.17270.43.1.2.1.0)</li>
	 * <li>S3) Verify retrieving the value of
	 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMLogsUploadStatus
	 * using SNMP OID (1.3.6.1.4.1.17270.43.1.2.2.0) to check Triggered State</li>
	 * <li>S4) Verify retrieving the value of
	 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMLogsUploadStatus
	 * using SNMP OID (1.3.6.1.4.1.17270.43.1.2.2.0) to check Failed State</li>
	 * <li>S5) Verify removing the dns names in the host entry file</li>
	 * <li>S6) Verify setting the value of
	 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMUploadLogsNow
	 * to True using SNMP OID (1.3.6.1.4.1.17270.43.1.2.1.0)</li>
	 * <li>S7) Verify retrieving the value of
	 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMLogsUploadStatus
	 * using SNMP OID (1.3.6.1.4.1.17270.43.1.2.2.0) to check Triggered State</li>
	 * <li>S8) Verify retrieving the value of
	 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMLogsUploadStatus
	 * using SNMP OID (1.3.6.1.4.1.17270.43.1.2.2.0) to check Failed State</li>
	 * <li>S9) Verify retrieving the value of
	 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMLogsUploadStatus
	 * using SNMP OID (1.3.6.1.4.1.17270.43.1.2.2.0) to check Complete State</li>
	 * </ol>
	 *
	 * @author Praveen Chandru
	 * @refactor yamini.s
	 * 
	 * @param device {@link Settop}
	 * 
	 * 
	 */

	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = BroadBandTestGroup.SYSTEM)
	@TestDetails(testUID = "TC-RDKB-LOGUPLOAD-1006")
	public void addHostEntryusingSnmp(Dut device) {
		// String to store the test case ID
		String testId = "TC-RDKB-LOGUPLOAD-106";
		// String to store the test step number
		String stepNumber = null;
		// String to store the error message
		String errorMessage = null;
		// stores the test status
		boolean status = false;
		// stores the snmpset output
		String snmpSetResponse = null;

		try {

			LOGGER.info("################### STARTING PRE-CONDITION ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			LOGGER.info("CHECK IF THE LOG UPLOAD IS SUCCESSFULL");
			LOGGER.info("##################################################################################");
			LOGGER.info(
					"PRE-CONDITION 1: DESCRIPTION : Verify setting the value of Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMUploadLogsNow to True using SNMP OID (1.3.6.1.4.1.17270.43.1.2.1.0)");
			LOGGER.info(
					"PRE-CONDITION 1: ACTION :Execute snmp mib 1.3.6.1.4.1.17270.43.1.2.1.0 and verify UploadLogsNow status");
			LOGGER.info(
					"PRE-CONDITION 1:EXPECTED : Should be able to set the value of UploadLogsNow parameter to True using Snmp Command");
			LOGGER.info("##################################################################################");
			snmpSetResponse = BroadBandSnmpUtils.retrieveSnmpSetOutputWithDefaultIndexOnRdkDevices(device, tapEnv,
					BroadBandSnmpMib.ECM_UPLOAD_LOG_NOW.getOid(), SnmpDataType.INTEGER,
					BroadBandTestConstants.STRING_VALUE_ONE);

			status = CommonMethods.isNotNull(snmpSetResponse)
					&& snmpSetResponse.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_ONE);
			errorMessage = "Snmpset failed to set the value to True using the OID";
			if (status) {
				LOGGER.info(
						"PRE-CONDITION 1: ACTUAL : Succesfully set UploadLogsNow parameter to True using Snmp Command");
			} else {
				LOGGER.error("PRE-CONDITION 1: ACTUAL : " + errorMessage);
				throw new Exception(
						BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION  : FAILED : " + errorMessage);
			}
			LOGGER.info("##################################################################################");

			LOGGER.info(
					"PRE-CONDITION 2: DESCRIPTION : Verify retrieving the value of Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMLogsUploadStatus using SNMP OID (1.3.6.1.4.1.17270.43.1.2.2.0) to check Complete State");
			LOGGER.info(
					"PRE-CONDITION 2: ACTION :Execute snmp mib 1.3.6.1.4.1.17270.43.1.2.2.0 and verify UploadLogsNow status");
			LOGGER.info("PRE-CONDITION 2:EXPECTED : Value of UploadLogsStatus Snmp OID should move to Complete State");
			LOGGER.info("##################################################################################");
			status = BroadBandUploadLogsUtils.verifyLogUploadStatusUsingSnmp(device, tapEnv,
					BroadBandTestConstants.STATUS_COMPLETE);
			errorMessage = "Upload Status did not move to Complete State";

			if (status) {
				LOGGER.info(
						"PRE-CONDITION 2: ACTUAL : Successfully verified Value of UploadLogsStatus Snmp OID moved to Complete State");
			} else {
				LOGGER.error("PRE-CONDITION 2: ACTUAL : " + errorMessage);
				throw new Exception(
						BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION  : FAILED : " + errorMessage);
			}
			LOGGER.info("##################################################################################");

			LOGGER.info("PRE-CONDITION 3: DESCRIPTION : Verify Success Log message in ConsoleLog/ArmConsoleLog.txt");
			LOGGER.info(
					"PRE-CONDITION 3: ACTION :Execute command: grep -i \"LOGS UPLOADED SUCCESSFULLY, RETURN CODE: 200\" in Consolelog.txt.0/ArmConsoleLog.txt");
			LOGGER.info("PRE-CONDITION 3:EXPECTED : LOGS UPLOADED SUCCESSFULLY message should be present.");
			LOGGER.info("##################################################################################");

			snmpSetResponse = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandTraceConstants.LOG_MESSAGE_LOGS_UPLOADED_SUCCESSFULLY,
					(CommonMethods.isAtomSyncAvailable(device, tapEnv)
							? BroadBandTestConstants.RDKLOGS_LOGS_ARM_CONSOLE_0
							: BroadBandTestConstants.RDKLOGS_LOGS_CONSOLE_TXT_0),
					BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			errorMessage = "Unable to validate sucess message";
			status = CommonMethods.isNotNull(snmpSetResponse) && snmpSetResponse
					.contains(BroadBandTraceConstants.LOG_MESSAGE_LOGS_UPLOADED_SUCCESSFULLY.replaceAll("\"", ""));
			if (status) {
				LOGGER.info(
						"PRE-CONDITION 3: ACTUAL : Verifed Successfully the log message in ConsoleLog/ArmConsoleLog.txt");
			} else {
				LOGGER.error("PRE-CONDITION 3: ACTUAL : " + errorMessage);
				throw new Exception(
						BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION  : FAILED : " + errorMessage);
			}
			LOGGER.info("##################################################################################");

			LOGGER.info("################### COMPLETED PRE-CONDITION ###################");

			/**
			 * 
			 * S1 : Add the following dns names in host entry (/etc/hosts)
			 * 
			 */
			stepNumber = "s1";
			status = false;
			LOGGER.info("##################################################################################");
			LOGGER.info("S1: Verify adding the dns names in the host entry file");
			LOGGER.info("##################################################################################");
			status = BroadBandUploadLogsUtils.addDnsNamesToHostsFile(device, tapEnv);
			errorMessage = "DNS names are not added in Hosts file";
			LOGGER.info("S1 ACTUAL: " + (status ? "DNS names are added in Hosts file" : errorMessage));
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, true);

			/**
			 * S2: Verify setting the value of
			 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.
			 * xOpsDMUploadLogsNow to True using SNMP OID *(1.3.6.1.4.1.17270.43.1.2.1.0)
			 */
			stepNumber = "s2";
			status = false;
			LOGGER.info("##################################################################################");
			LOGGER.info(
					"S2: Verify setting the value of Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMUploadLogsNow to True using SNMP OID (1.3.6.1.4.1.17270.43.1.2.1.0)");
			LOGGER.info(
					"EXPECTED : Should be able to set the value of UploadLogsNow parameter to True using Snmp Command");
			LOGGER.info("##################################################################################");
			snmpSetResponse = BroadBandSnmpUtils.retrieveSnmpSetOutputWithDefaultIndexOnRdkDevices(device, tapEnv,
					BroadBandSnmpMib.ECM_UPLOAD_LOG_NOW.getOid(), SnmpDataType.INTEGER,
					BroadBandTestConstants.STRING_VALUE_ONE);

			status = CommonMethods.isNotNull(snmpSetResponse)
					&& snmpSetResponse.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_ONE);
			errorMessage = "Snmpset failed to set the value to True using the OID";
			LOGGER.info("S2 ACTUAL: " + (status ? "Snmpset operation is successful" : errorMessage));
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, ErrorType.SNMP + errorMessage, true);

			/**
			 * S3: Verify retrieving the value of
			 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.
			 * xOpsDMLogsUploadStatus using SNMP OID (1.3.6.1.4.1.17270.43.1.2.2.0) to check
			 * the status moved to Triggered State
			 */
			stepNumber = "s3";
			status = false;
			LOGGER.info("##################################################################################");
			LOGGER.info(
					"S3: Verify retrieving the value of Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMLogsUploadStatus using SNMP OID (1.3.6.1.4.1.17270.43.1.2.2.0) to check Triggered State");
			LOGGER.info("EXPECTED : Value of UploadLogsStatus Snmp OID should move to Triggered State");
			LOGGER.info("##################################################################################");

			status = BroadBandUploadLogsUtils.verifyLogUploadStatusUsingSnmp(device, tapEnv,
					BroadBandTestConstants.STATUS_TRIGGERED);
			errorMessage = "Upload Status did not move to Triggered State";
			LOGGER.info("S3 ACTUAL: " + (status ? "Upload Status moved to Triggered State" : errorMessage));
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, ErrorType.SNMP + errorMessage, false);

			/**
			 * S4: Verify retrieving the value of
			 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.
			 * xOpsDMLogsUploadStatus using SNMP OID (1.3.6.1.4.1.17270.43.1.2.2.0) to check
			 * the status moved to In Progress State
			 */
			stepNumber = "s4";
			status = false;
			LOGGER.info("##################################################################################");
			LOGGER.info(
					"S4: Verify retrieving the value of Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMLogsUploadStatus using SNMP OID (1.3.6.1.4.1.17270.43.1.2.2.0) to check In Progress State");
			LOGGER.info("EXPECTED : Value of UploadLogsStatus Snmp OID should move to In Progress State");
			LOGGER.info("##################################################################################");

			status = BroadBandUploadLogsUtils.verifyLogUploadStatusUsingSnmp(device, tapEnv,
					BroadBandTestConstants.STATUS_INPROGRESS);
			errorMessage = "Upload Status did not move to In Progress State";
			LOGGER.info("S4 ACTUAL: " + (status ? "Upload Status moved to In Progress State" : errorMessage));
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, ErrorType.SNMP + errorMessage, false);

			/**
			 * S5: Verify retrieving the value of
			 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.
			 * xOpsDMLogsUploadStatus using SNMP OID (1.3.6.1.4.1.17270.43.1.2.2.0) to check
			 * the status moved to Failed State
			 */

			stepNumber = "s5";
			status = false;
			LOGGER.info("##################################################################################");
			LOGGER.info(
					"S5: Verify retrieving the value of Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMLogsUploadStatus using SNMP OID (1.3.6.1.4.1.17270.43.1.2.2.0) to check Failed State");
			LOGGER.info("EXPECTED : Value of UploadLogsStatus Snmp OID should move to Failed State");
			LOGGER.info("##################################################################################");

			status = BroadBandUploadLogsUtils.verifyLogUploadStatusUsingSnmp(device, tapEnv,
					BroadBandTestConstants.STATUS_FAILED);
			errorMessage = "Upload Status did not move to Failed State";
			LOGGER.info("S5 ACTUAL: " + (status ? "Upload Status moved to Failed State" : errorMessage));
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, ErrorType.SNMP + errorMessage, false);

			/**
			 * S6: Verify removing the dns names in the host entry file
			 *
			 */

			stepNumber = "s6";
			status = false;
			LOGGER.info("##################################################################################");
			LOGGER.info("S6: Verify removing the dns names in the host entry file");
			LOGGER.info("##################################################################################");
			status = BroadBandUploadLogsUtils.removeDnsNamesFromHostsFile(device, tapEnv);
			errorMessage = "DNS names are not removed in Hosts file";
			LOGGER.info("S6 ACTUAL: " + (status ? "DNS names are removed in Hosts file" : errorMessage));
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, true);

			/**
			 * S7: Verify setting the value of
			 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.
			 * xOpsDMUploadLogsNow to True using SNMP OID *(1.3.6.1.4.1.17270.43.1.2.1.0)
			 */
			stepNumber = "s7";
			status = false;
			LOGGER.info("##################################################################################");
			LOGGER.info(
					"S7: Verify setting the value of Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMUploadLogsNow to True using SNMP OID (1.3.6.1.4.1.17270.43.1.2.1.0)");
			LOGGER.info(
					"EXPECTED : Should be able to set the value of UploadLogsNow parameter to True using Snmp Command");
			LOGGER.info("##################################################################################");
			snmpSetResponse = BroadBandSnmpUtils.retrieveSnmpSetOutputWithDefaultIndexOnRdkDevices(device, tapEnv,
					BroadBandSnmpMib.ECM_UPLOAD_LOG_NOW.getOid(), SnmpDataType.INTEGER,
					BroadBandTestConstants.STRING_VALUE_ONE);
			status = CommonMethods.isNotNull(snmpSetResponse)
					&& snmpSetResponse.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_ONE);
			errorMessage = "Snmpset failed to set the value to True using the OID";
			LOGGER.info("S7 ACTUAL: " + (status ? "Snmpset operation is successful" : errorMessage));
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, ErrorType.SNMP + errorMessage, true);

			/**
			 * S8: Verify retrieving the value of
			 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.
			 * xOpsDMLogsUploadStatus using SNMP OID (1.3.6.1.4.1.17270.43.1.2.2.0) to check
			 * the status moved to Triggered State
			 */
			stepNumber = "s8";
			status = false;
			LOGGER.info("##################################################################################");
			LOGGER.info(
					"S8: Verify retrieving the value of Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMLogsUploadStatus using SNMP OID (1.3.6.1.4.1.17270.43.1.2.2.0) to check Triggered State");
			LOGGER.info("EXPECTED : Value of UploadLogsStatus Snmp OID should move to Triggered State");
			LOGGER.info("##################################################################################");

			status = BroadBandUploadLogsUtils.verifyLogUploadStatusUsingSnmp(device, tapEnv,
					BroadBandTestConstants.STATUS_TRIGGERED);
			errorMessage = "Upload Status did not move to Triggered State";
			LOGGER.info("S8 ACTUAL: " + (status ? "Upload Status moved to Triggered State" : errorMessage));
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, ErrorType.SNMP + errorMessage, false);

			/**
			 * S9: Verify retrieving the value of
			 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.
			 * xOpsDMLogsUploadStatus using SNMP OID (1.3.6.1.4.1.17270.43.1.2.2.0) to check
			 * the status moved to In Progress State
			 */
			stepNumber = "s9";
			status = false;
			LOGGER.info("##################################################################################");
			LOGGER.info(
					"S9: Verify retrieving the value of Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMLogsUploadStatus using SNMP OID (1.3.6.1.4.1.17270.43.1.2.2.0) to check In Progress State");
			LOGGER.info("EXPECTED : Value of UploadLogsStatus Snmp OID should move to In Progress State");
			LOGGER.info("##################################################################################");

			status = BroadBandUploadLogsUtils.verifyLogUploadStatusUsingSnmp(device, tapEnv,
					BroadBandTestConstants.STATUS_INPROGRESS);
			errorMessage = "Upload Status did not move to In Progress State";
			LOGGER.info("S9 ACTUAL: " + (status ? "Upload Status moved to In Progress State" : errorMessage));
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, ErrorType.SNMP + errorMessage, false);

			/**
			 * S10: Verify retrieving the value of
			 * Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.
			 * xOpsDMLogsUploadStatus using SNMP OID (1.3.6.1.4.1.17270.43.1.2.2.0) to check
			 * the status moved to Complete State
			 */
			stepNumber = "s10";
			status = false;
			LOGGER.info("##################################################################################");
			LOGGER.info(
					"S10: Verify retrieving the value of Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMLogsUploadStatus using SNMP OID (1.3.6.1.4.1.17270.43.1.2.2.0) to check Complete State");
			LOGGER.info("EXPECTED : Value of UploadLogsStatus Snmp OID should move to Complete State");
			LOGGER.info("##################################################################################");

			status = BroadBandUploadLogsUtils.verifyLogUploadStatusUsingSnmp(device, tapEnv,
					BroadBandTestConstants.STATUS_COMPLETE);
			errorMessage = "Upload Status did not move to Complete State";
			LOGGER.info("S10 ACTUAL: " + (status ? "Upload Status moved to Complete State" : errorMessage));
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, ErrorType.SNMP + errorMessage, true);

		}

		catch (Exception exception) {
			errorMessage = "Exception occured while verifying the uploadstatus Snmp OID after adding the dns names for blocking the log upload: "
					+ exception.getMessage();
			LOGGER.error(errorMessage);
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);
		}

		LOGGER.info("ENDING TEST CASE: TC-RDKB-LOGUPLOAD-1006");
	}

}
