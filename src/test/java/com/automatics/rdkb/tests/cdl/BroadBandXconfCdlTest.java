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

package com.automatics.rdkb.tests.cdl;

import java.util.Map;

import org.apache.http.HttpStatus;
import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.AutomaticsConstants;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandCdlConstants;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandPropertyKeyConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.RDKBTestConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandPostConditionUtils;
import com.automatics.rdkb.utils.BroadBandPreConditionUtils;
import com.automatics.rdkb.utils.BroadBandRfcFeatureControlUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.cdl.BroadBandCodeDownloadUtils;
import com.automatics.rdkb.utils.cdl.BroadBandXconfCdlUtils;
import com.automatics.rdkb.utils.cdl.CodeDownloadUtils;
import com.automatics.rdkb.utils.cdl.FirmwareDownloadUtils;
import com.automatics.rdkb.utils.dmcli.DmcliUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;
import com.automatics.utils.xconf.XConfUtils;

/**
 * Class for XCONF CDL tests
 * 
 * @author anandam.s
 *
 */

public class BroadBandXconfCdlTest extends AutomaticsTestBase {

	/**
	 * Trigger XCONF/DIFD HTTP CDL during router mode [ Deferred Reboot] and check
	 * Partner ID
	 * <ol>
	 * <li>PRE-CONDITION 1: To disable code big first</li>
	 * <li>1.Set lanMode to router</li>
	 * <li>2.Check Whether Device within Maintaince Window and if not ,Simulate the
	 * device to fall within maintaince window by modifing the Maintaince window
	 * start and end time using RFC.</li>
	 * <li>3.Configure XCONF server with required configuration using REST API</li>
	 * <li>4.Reboot the device to initiate XCONF download.</li>
	 * <li>5.verify the activation in progress has valid value.</li>
	 * <li>6.verify the delay download log is logged in xconf.txt.0.</li>
	 * <li>7.Verify XCONF server response</li>
	 * <li>8.Verify whether download request is accepted</li>
	 * <li>9.Verify code download successful</li>
	 * <li>10.Verify whether device is rebooted after successful download in
	 * Maintenance window</li>
	 * <li>11.Verify current image name from version.txt</li>
	 * <li>12.Verify last reboot reason (only if the device is in Maintenance
	 * window)</li>
	 * <li>13.Verify whether the Xconf query has Partner ID in 'xconf.txt.0' log
	 * file.</li>
	 * <li>14.Verify whether the splunk query has the Partner ID.</li>
	 * <li>POST-CONDITION 1: Remove tmp xconf files.</li>
	 * <li>POST-CONDITION 2 : VERIFY THAT DEVICE IS UPGRADED WITH THE PREVIOUS
	 * FIRMWARE VERSION</li>
	 * <li>POST-CONDITION 3 : Reset FirmwareUpgradeStartTime to default</li>
	 * <li>POST-CONDITION 4 : Reset FirmwareUpgradeEndTime to default</li>
	 * </ol>
	 * 
	 * @param device instance of {@link Dut}
	 * 
	 * @author styles mangalasseri, Vignesh
	 * @refactor anandam
	 */
	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true)
	@TestDetails(testUID = "TC-RDKB-XCONF-1001", testDecription = "Verify XCONF/DIFD HTTP CDL during router mode [ Deferred Reboot] and check Partner ID")
	public void testToVerifyXconfDiffereredRebootHttpCdl(Dut device) {

		// execution status
		boolean status = false;
		// Test case id
		String testCaseId = "TC-RDKB-XCONF-001";
		// step number
		int stepNumber = BroadBandTestConstants.CONSTANT_1;
		// variable to hold the server response
		// String response = null;
		String currentImageName = null;
		// image name to perform CDL
		String imageNameForCdl = null;
		// reboot immediately status
		String rebootImmediately = "false";
		// error message
		String errorMessage = null;
		// variable to hold the expected log message for validation
		String expectedLogMessage = null;
		// test step number
		String stepNum = "S" + stepNumber;
		String response = null;
		boolean isCdlDataPosted = false;
		boolean hasLatestBuildChanged = false;
		boolean logFileExists = false;
		String expectedPartnerId = null;
		String partnerId = null;
		long startTime = 0L;
		// cdl log for validation
		String cdlLogsForValidation = null;
		String maintenanceWindowStartTime = null;
		String maintenanceWindowEndTime = null;
		// int NUMBER_OF_XCONF_OR_DCM_QUERY_PARAMETERS = 4;
		String buildExtension = null;

		/** splunk start index **/
		// String SPLUNK_START_INDEX = "20m";

		/** Wait Time **/
		long waitTime = 15;
		/** Constant holds the maintenance window mapping **/
		Map<String, String> maintenanceWindow = null;

		try {
			LOGGER.info("#######################################################################################");
			LOGGER.info("STARTING TEST CASE -TC-RDKB-XCONF-1001");
			LOGGER.info(
					"TEST DESCRIPTION: Trigger XCONF/DIFD HTTP CDL during router mode [ Deferred Reboot] and check Partner ID");
			LOGGER.info("PRE-CONDITION STEPS");
			LOGGER.info("PRE-CONDITION 1 : To disable code big first");
			LOGGER.info("TEST STEPS : ");
			LOGGER.info("1. set lanMode to router");
			LOGGER.info(
					"2. Check Whether Device within Maintaince Window  and Simulate the device to fall within maintaince window by modifing the Maintaince window start and end time using RFC.");
			LOGGER.info("3.Configure XCONF server with required configuration using REST API");
			LOGGER.info("4. Reboot the device to initiate XCONF download");
			LOGGER.info("5 : verify the activation In progress has valid value");
			LOGGER.info("6 : verify the delay download log is logged in xconf.txt.0");
			LOGGER.info("7. Verify XCONF server response");
			LOGGER.info("8. Verify whether download request is accepted");
			LOGGER.info("9. Verify code download successful");
			LOGGER.info("10. Verify whether device is rebooted after successful download in Maintainance Window");
			LOGGER.info("11. Verify current image name from version.txt");
			LOGGER.info("12. Verify last reboot reason (only if the device is in Maintenance window)");
			LOGGER.info("13. Verify whether the Xconf query has Partner ID in 'xconf.txt.0' log file");
			/* LOGGER.info("14. Verify whether the splunk query has the Partner ID"); */
			LOGGER.info("POST-CONDITION STEPS");
			LOGGER.info("POST-CONDITION 1 : TO CLEAR THE CDL INFORMATION URL");
			LOGGER.info("POST-CONDITION 2 : VERIFY THAT DEVICE IS UPGRADED WITH THE PREVIOUS FIRMWARE VERSION");
			LOGGER.info("POST-CONDITION 3 : Reset FirmwareUpgradeStartTime to default");
			LOGGER.info("POST-CONDITION 4 : Reset FirmwareUpgradeEndTime to default");
			LOGGER.info("#######################################################################################");

			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			/**
			 * PRE-CONDITION 1 : Pre-Condition method to disable code big first.
			 */
			BroadBandPreConditionUtils.executePreConditionToDisableCodeBigFirst(device, tapEnv,
					BroadBandTestConstants.CONSTANT_1);
			LOGGER.info("#######################################################################################");

			/**
			 * STEP 1 : SET RDKB DEVICE LANMODE TO ROUTER
			 */
			stepNum = "S" + stepNumber;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Set rdkb device lanmode to router");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Execute webpa command : Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanMode with value as router");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : Should be able to set lanMode as router");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to set the lan mode to router";
			try {
				status = BroadBandCommonUtils.setDeviceInRouterModeStatusUsingWebPaCommand(tapEnv, device);
			} catch (Exception e) {
				errorMessage = "Exception occured while setting the lan mode to router " + e.getMessage();
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL: Successfully verified the device lan mode as router");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL :" + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			// Step 2
			stepNumber++;
			stepNum = "S" + stepNumber;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Check Whether Device within Maintaince Window  and Simulate the device to fall within maintaince window by modifing the Maintaince window start and end time using RFC.");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : 1.Copy and update /nvram/rfc.properties with mock RFC config server URL \r\n"
					+ "2.Post payload after replacing ESTB mac and enable/disable value to RFC server");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : Successfully configured tthe Device MaintenanceWindow using RFC");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Failed to verify decice acessibility";
			maintenanceWindowStartTime = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_COMMAND_MAINTENANCE_WINDOW_START_TIME);
			maintenanceWindowEndTime = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_COMMAND_MAINTENANCE_WINDOW_END_TIME);

			// Setting the device to fall in maintenance window
			try {
				maintenanceWindow = BroadBandCommonUtils
						.calculateFirmwareUpgradeMaintenanceWindowWithTimeInterval(tapEnv, device, waitTime);
				status = BroadBandCommonUtils.calculateAndSetMaintanenceWindow(device, tapEnv, waitTime,
						maintenanceWindow);
			} catch (Exception e) {
				status = false;
				errorMessage += "Exception occured while setting the maintenance window ." + e.getMessage();
			}
			BroadBandCommonUtils.getMaintenanceWindowDetails(device, tapEnv);

			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ ": ACTUAL: Successfully configured tthe Device Maintenance window using RFC");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL :" + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			/**
			 * STEP 3 : CONFIGURE XCONF SERVER WITH REQUIRED CONFIGURATION USING REST API
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Configure XCONF server with required configuration using REST API with delaydownload 5 mins");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Create /nvram/swupdate.conf file and add the software update url.");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : Should be able to configure the code download configuration using XCONF Rest API and swupdate.conf should updated with xconf url");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Failed to configure XCONF server with required configuration using REST API";
			try {
				currentImageName = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device);
				LOGGER.info("CURRENT FIRMWARE VERSION: " + currentImageName);
				// add signed extension to the image
				buildExtension = BroadBandTestConstants.SIGNED_BUILD_IMAGE_EXTENSION;
				try {
					buildExtension = BroadBandCommonUtils.getAutomaticsPropsValueByResolvingPlatform(device,
							BroadBandTestConstants.PROP_KEY_SIGNED_EXTENSION);
				} catch (Exception e) {
					buildExtension = BroadBandTestConstants.SIGNED_BUILD_IMAGE_EXTENSION;
					LOGGER.info("No platform dpeendent extensions are mentioned in  automatics properties");

				}

				imageNameForCdl = tapEnv.getLatestBuildImageVersionForCdlTrigger(device, false);
				LOGGER.info("LATEST FIRMWARE VERSION: " + imageNameForCdl);
				if (CommonMethods.isNull(imageNameForCdl)) {
					LOGGER.info(
							" GA image obtained from deployed version service is null. Hence getting the image from property file ");
					imageNameForCdl = BroadBandCommonUtils.getAutomaticsPropsValueByResolvingPlatform(device,
							BroadBandTestConstants.PARTIAL_PROPERTY_KEY_FOR_GA_IMAGE);
					LOGGER.info("Latest Firmware version from property file: " + imageNameForCdl);
				}

				if (CommonMethods.isNotNull(imageNameForCdl)) {
					if (!imageNameForCdl.contains(currentImageName)) {
						BroadBandXconfCdlUtils.configureRdkbDeviceForXconfCdl(tapEnv, device, imageNameForCdl, false,
								BroadBandTestConstants.FIRMWARE_DOWNLOAD_PROTOCOL_HTTP,
								BroadBandTestConstants.CONSTANT_5);
						status = true;
						isCdlDataPosted = status;
					} else {
						errorMessage = "Device current image and latest image to upgrade are same.";
					}

				} else {
					errorMessage = "Unable to retrieve latest image name for RDKB devices";
				}
				tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_GET_XCONF_TXT);
			} catch (Exception e) {
				errorMessage = "Exception occured while configuring xconf server " + e.getMessage();
				LOGGER.error(errorMessage);
			}

			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ ": ACTUAL: Successfully Configured XCONF server with required configuration.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL :" + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			/**
			 * STEP 4 : INITIATE CDL USING WEBPA COMMAND
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Initiate cdl using webpa command");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : EXECUTE WEBPA COMMAND : Device.X_COMCAST-COM_Xcalibur.Client.xconfCheckNow to true.");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : Should be able to Initiate cdl using webpa command");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Failed to initiate cdl using webpa command";
			try {
				status = BroadBandXconfCdlUtils.initiateXconfCdlThroughWebpa(tapEnv, device);
			} catch (Exception e) {
				errorMessage = "Exception occured while initiating cdl throgh webpa " + e.getMessage();
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL: Successfully initiated cdl using webpa command.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL :" + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			/**
			 * STEP 5 : Verify the activation In progress has valid value
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			errorMessage = "Failed to verify activation In Progress value";
			status = false;

			LOGGER.info("**********************************************************************************");

			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : verify the activation In progress has valid value");

			LOGGER.info(
					"STEP " + stepNumber + ": ACTION :  grep -I \"activationInProgress=\" /rdklogs/logs/xconf.txt.0");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : activationInProgress=false log should be present in xconf.log");
			LOGGER.info("**********************************************************************************");
			boolean activationValue = false;
			try {
				logFileExists = CommonMethods.isFileExists(device, tapEnv, BroadBandCommandConstants.FILE_TMP_XCONFCDL);
				LOGGER.info(BroadBandCommandConstants.FILE_TMP_XCONFCDL + " Log file status " + logFileExists);
				response = tapEnv.executeWebPaCommand(device,
						BroadBandWebPaConstants.WEBPA_PARAM_FOR_SYNDICATION_PARTNER_ID);
				activationValue = CommonUtils.patternSearchFromTargetString(response,
						BroadBandTestConstants.UNKNOWN_REBOOT_REASON) ? BroadBandTestConstants.BOOLEAN_VALUE_TRUE
								: BroadBandTestConstants.BOOLEAN_VALUE_FALSE;
				response = CommonUtils.searchLogFilesWithPollingInterval(tapEnv, device,
						BroadBandTestConstants.STRING_ACTIVATION_INPROGRESS,
						BroadBandCommandConstants.STRING_XCONF_BACKUP_FILEPATH,
						BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			} catch (Exception e) {
				LOGGER.error("Exception occured while executing command:" + e.getMessage());
			}
			status = CommonMethods.isNotNull(response)
					&& CommonUtils.patternSearchFromTargetString(response,
							BroadBandCommonUtils.concatStringUsingStringBuffer(
									BroadBandTestConstants.STRING_ACTIVATION_INPROGRESS,
									String.valueOf(activationValue)));
			if (!status && logFileExists) {
				response = CommonUtils.searchLogFilesWithPollingInterval(tapEnv, device,
						BroadBandCommonUtils.concatStringUsingStringBuffer(
								BroadBandTestConstants.STRING_ACTIVATION_INPROGRESS, String.valueOf(activationValue)),
						BroadBandCommandConstants.FILE_TMP_XCONFCDL, BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
						BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
				LOGGER.info("Log in tmp file " + response);
				status = CommonMethods.isNotNull(response)
						&& CommonUtils.patternSearchFromTargetString(response,
								BroadBandCommonUtils.concatStringUsingStringBuffer(
										BroadBandTestConstants.STRING_ACTIVATION_INPROGRESS,
										String.valueOf(activationValue)));
			}

			if (status) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL : Successfully validated activationInProgress="
						+ activationValue + " in xconf");
			} else {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			/**
			 * STEP 6 : verify the delay download log is logged in xconf.txt.0
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			errorMessage = "Failed to verify the delay download log message";
			status = false;
			response = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP " + stepNumber + ": DESCRIPTION : verify the delay download log is logged in xconf.txt.0");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : grep -I \"Device configured with download delay of 5 minutes\" /rdklogs/logs/xconf.txt.0");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : Device configured with download delay of 5 minutes log should be present in xconf.txt.0");
			LOGGER.info("**********************************************************************************");
			try {
				response = CommonUtils.searchLogFilesWithPollingInterval(tapEnv, device,
						BroadBandTestConstants.STRING_FOR_DELAY_DOWNLOAD_SPECIFIED_TIME
								.replace(BroadBandTestConstants.STRING_REPLACE, BroadBandTestConstants.STRING_5),
						BroadBandCommandConstants.STRING_XCONF_BACKUP_FILEPATH,
						BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			} catch (Exception e) {
				LOGGER.error("Exception occured while executing command:" + e.getMessage());
			}
			status = CommonMethods.isNotNull(response) && CommonUtils.patternSearchFromTargetString(response,
					BroadBandTestConstants.STRING_FOR_DELAY_DOWNLOAD_SPECIFIED_TIME
							.replace(BroadBandTestConstants.STRING_REPLACE, BroadBandTestConstants.STRING_5));
			if (!status && logFileExists) {
				response = CommonUtils.searchLogFilesWithPollingInterval(tapEnv, device,
						BroadBandTestConstants.STRING_FOR_DELAY_DOWNLOAD_SPECIFIED_TIME
								.replace(BroadBandTestConstants.STRING_REPLACE, BroadBandTestConstants.STRING_5),
						BroadBandCommandConstants.FILE_TMP_XCONFCDL, BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
						BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
				status = CommonMethods.isNotNull(response) && CommonUtils.patternSearchFromTargetString(response,
						BroadBandTestConstants.STRING_FOR_DELAY_DOWNLOAD_SPECIFIED_TIME
								.replace(BroadBandTestConstants.STRING_REPLACE, BroadBandTestConstants.STRING_5));

			}

			if (status) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL : Successfully validated delay download text");
			} else {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			/**
			 * STEP 7 : VERIFY XCONF FIRMWARE CONFIGURATION
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify XCONF firmware configuration");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : EXECUTE WEBPA COMMAND : Device.X_COMCAST-COM_Xcalibur.Client.xconfCheckNow to true.");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : /rdklogs/logs/xconf.txt.0 file should contains the configuration as configured");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Failed to initiate cdl using webpa command";
			try {

				tapEnv.waitTill(BroadBandTestConstants.FIVE_MINUTES);

				cdlLogsForValidation = BroadBandXconfCdlUtils.getCdlLogsForValidation(tapEnv, device);

				if (CommonMethods.isNotNull(cdlLogsForValidation)) {
					BroadBandXconfCdlUtils.retrieveAndValidateXconfConfiguration(cdlLogsForValidation, imageNameForCdl,
							rebootImmediately, BroadBandTestConstants.FIRMWARE_DOWNLOAD_PROTOCOL_HTTP);
					status = true;
				} else {
					errorMessage = "Unable to read the contents from "
							+ BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0;
				}
			} catch (Exception e) {
				errorMessage = e.getMessage();
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ ": ACTUAL: Successfully validated that the XCONF configurations(download protocol, Current version and Upgrade version) are parsed successfully and logged in "
						+ BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0);
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL :" + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			/**
			 * STEP 8 : VERIFY WHETHER DOWNLOAD REQUEST IS ACCEPTED
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify whether download request is accepted.");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : check the download accepted message in /rdklogs/logs/xconf.txt.0 .");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : /rdklogs/logs/xconf.txt.0 file should contains the accepted message in the format of \""
					+ BroadBandCdlConstants.ACCEPTED_MESSAGE_FOR_XCONF_CODE_DOWNLOAD + "\"");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Failed to verify download request";
			try {
				// format current image name which is retrieved using webpa command to compare
				// with current image
				// version
				currentImageName = BroadBandCommonUtils
						.removeDifferentSignedExtensionsInRequestedBuildName(currentImageName);
				imageNameForCdl = BroadBandCommonUtils
						.removeDifferentSignedExtensionsInRequestedBuildName(imageNameForCdl);
				// verify the rejection message
				expectedLogMessage = BroadBandCdlConstants.ACCEPTED_MESSAGE_FOR_XCONF_CODE_DOWNLOAD
						.replaceAll(BroadBandCdlConstants.CONFIGURATION_CURRENT_IMAGE_NAME, currentImageName)
						.replaceAll(BroadBandCdlConstants.CONFIGURATION_REQUESTED_IMAGE_NAME, imageNameForCdl);
				// workaround to overcome the type error in RDK Code
				cdlLogsForValidation = cdlLogsForValidation.replaceAll("imgae", "image");
				status = cdlLogsForValidation.toLowerCase().contains(expectedLogMessage.toLowerCase());
				errorMessage = "Unable to find the XCONF " + BroadBandTestConstants.FIRMWARE_DOWNLOAD_PROTOCOL_HTTP
						+ " CDL accepted message '" + expectedLogMessage + "' in "
						+ BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0;
			} catch (Exception e) {
				errorMessage = e.getMessage();
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ ": ACTUAL: Successfully validated the XCONF HTTP CDL accepted message in "
						+ BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0);
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL :" + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			/**
			 * STEP 9 : VERIFY CODE DOWNLOAD IS SUCCESSFUL
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			status = false;
			// variable to block test script execution
			boolean blockExecution = false;
			expectedLogMessage = FirmwareDownloadUtils.getCdlDownloadSuccessLog(device);
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify code download is successful.");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : check the download successfull message in /rdklogs/logs/xconf.txt.0 .");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : " + BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0
					+ " file should contains the code download success message in the format of \"" + expectedLogMessage
					+ "\"");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Failed to verify code download successfull status";
			try {
				if (CommonMethods.isNotNull(cdlLogsForValidation)) {
					status = cdlLogsForValidation.toLowerCase().contains(expectedLogMessage.toLowerCase());
					errorMessage = "Unable to find the XCONF HTTP CDL success message " + expectedLogMessage + " in "
							+ BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0;
				} else if (CommonMethods.isNotNull(cdlLogsForValidation) && cdlLogsForValidation.toLowerCase()
						.contains(BroadBandCdlConstants.XCONF_HTTP_DOWNLOAD_NOT_SUCCESSFUL.toLowerCase())) {
					blockExecution = true;
					errorMessage = "XCONF HTTP CDL not successful. obtained "
							+ BroadBandCdlConstants.XCONF_HTTP_DOWNLOAD_NOT_SUCCESSFUL + " in "
							+ BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0;
				} else {
					errorMessage = "Unable to retrieve Code download completed message from "
							+ BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0;
				}
			} catch (Exception e) {
				errorMessage = e.getMessage();
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ ": ACTUAL: Successfully validated the XCONF HTTP CDL completed message in "
						+ BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0);
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL :" + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, blockExecution);

			// Step 10
			stepNumber++;
			stepNum = "S" + stepNumber;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : If Device Within Mainteince window, Check whether device getting rebooted within 1 min");
			LOGGER.info("STEP " + stepNumber + ": ACTION : Check device is acessible");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : Device should not be accessible.");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Failed to verify decice acessibility";
			status = BroadBandCommonUtils.verifyDeviceRebootDuringMaintenanceWindow(device, tapEnv, maintenanceWindow);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ ": ACTUAL: Successfully verified that device went for reboot within maintenanceWindow");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL :" + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			/**
			 * STEP 11 : VERIFY CURRENT IMAGE NAME FROM VERSION.TXT
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify current image name from version.txt");
			LOGGER.info("STEP " + stepNumber + ": ACTION : Retrive Current image name from version.txt");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : Current image name from version.txt on both ARM or ATOM should be same.");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Failed to verify current image name";
			try {
				LOGGER.info("Waiting 1 minute");
				tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
				FirmwareDownloadUtils.verifyCurrentImageNameBothInArmAndAtomConsole(tapEnv, device, imageNameForCdl);
				status = true;
				hasLatestBuildChanged = status;
			} catch (Exception e) {
				status = false;
				errorMessage = e.getMessage();
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ ": ACTUAL: Successfully validated that the current image name from version.txt on both ARM or ATOM are as expected.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL :" + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			/**
			 * STEP 12 : VERIFY CURRENT REBOOT REASON
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify current reboot reason");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : EXECUTE WEBPA COMMAND : Device.DeviceInfo.X_RDKCENTRAL-COM_LastRebootReason");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : WebPA Parameter should return the value");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Failed to verify last reboot reason.";
			status = FirmwareDownloadUtils.verifyLastRebootReasonViaWebpa(tapEnv, device,
					BroadBandCdlConstants.EXPECTED_LAST_REBOOT_REASON_STATUS_DIFD_CDL_VIA_WEBPA_REBOOT)
					|| FirmwareDownloadUtils.verifyLastRebootReasonViaWebpa(tapEnv, device,
							BroadBandCdlConstants.EXPECTED_LAST_REBOOT_REASON_STATUS_AFTER_CDL)
					|| FirmwareDownloadUtils.verifyLastRebootReasonViaWebpa(tapEnv, device,
							BroadBandTestConstants.REBOOT_REASON_RFC_REBOOT);
			if (status) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL: Successfully validated the reboot reason.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL :" + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			/**
			 * STEP 13 : Verify whether the Xconf query has Partner ID in \"xconf.txt.0\"
			 * log file.
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ " : DESCRIPTION : Verify whether the Xconf query has Partner ID in \"xconf.txt.0\" log file");
			LOGGER.info("STEP " + stepNumber
					+ " : ACTION : Execute command \"grep -i partnerId /rdklogs/logs/xconf.txt.0\" to verify partner ID in device console");
			LOGGER.info("STEP " + stepNumber
					+ " : EXPECTED : XCONF query should have partnerId parameter in xconf.txt.0 log file");
			LOGGER.info("**********************************************************************************");
			LOGGER.info("***** RETRIEVING THE PARTNER ID USING WEBPA/DMCLI! *****");
			errorMessage = "Unable to retrieve Partner ID using WebPA/DMCLI";
			try {
				partnerId = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_FOR_SYNDICATION_PARTNER_ID);
				if (CommonMethods.isNotNull(partnerId)) {
					expectedPartnerId = BroadBandCommonUtils.concatStringUsingStringBuffer("partnerId",
							AutomaticsConstants.DELIMITER_EQUALS, partnerId);
					LOGGER.info("***** EXEPECTED PARTNER ID STRING : " + expectedPartnerId);
					LOGGER.info("***** RETRIEVING THE PARTNER ID FROM /RDKLOGS/LOGS/XCONF.TXT.0! *****");
					errorMessage = "Unable to retrieve Partner ID from /rdklogs/logs/xconf.txt.0 !";
					startTime = System.currentTimeMillis();
					do {
						status = (cdlLogsForValidation.toLowerCase()).contains(expectedPartnerId.toLowerCase());
					} while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS
							&& !status && BroadBandCommonUtils.hasWaitForDuration(tapEnv,
									BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
				}
			} catch (Exception e) {
				LOGGER.info("Exception occurred while validating partner ID in Xconf.txt File : " + e.getMessage());
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ ": ACTUAL : Firmware download is triggered Successfully and XCONF query is also having expected Partner ID in xconf.txt.0");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

		} catch (Exception exception) {
			errorMessage = "Exception occurred during execution : " + exception.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					true);
		} finally {
			int postCondNumber = BroadBandTestConstants.CONSTANT_0;
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			tapEnv.executeCommandUsingSsh(device, "rm -rf /rdklogs/logs/xconf.txt");
			if (logFileExists) {
				postCondNumber++;
				LOGGER.info("####################################################################");
				LOGGER.info("POST-CONDITION " + postCondNumber + " : DESCRIPTION : Remove tmp xconf files");
				LOGGER.info("POST-CONDITION " + postCondNumber + " : ACTION : Remove "
						+ BroadBandCommandConstants.FILE_TMP_XCONFCDL);
				LOGGER.info("POST-CONDITION " + postCondNumber + " : EXPECTED : Files removed successfully");
				LOGGER.info("####################################################################");
				status = false;

				errorMessage = "Failed to remove /tmp/Consolelog.txt file";
				status = BroadBandCommonUtils.removeFileandVerifyFileRemoval(tapEnv, device,
						BroadBandCommandConstants.FILE_TMP_XCONFCDL);
				if (status) {
					LOGGER.info("POST-CONDITION " + postCondNumber + " : ACTUAL : File removed successfully");
				} else {
					LOGGER.error("POST-CONDITION " + postCondNumber + " : ACTUAL : " + errorMessage);
				}
			}
			/**
			 * POST CONDITION : POST-CONDITION METHOD TO CLEAR THE CDL INFORMATION URL
			 */
			if (isCdlDataPosted) {
				postCondNumber++;
				BroadBandPostConditionUtils.executePostConditionToClearCdlInfoInXconf(device, tapEnv, postCondNumber);
			}
			/**
			 * POST CONDITION : VERIFY THAT DEVICE IS UPGRADED WITH THE PREVIOUS FIRMWARE
			 * VERSION.
			 */
			LOGGER.info("BUILD TO REVERT :" + currentImageName);
			BroadBandPostConditionUtils.executePostConditionToTriggerCdl(device, tapEnv, hasLatestBuildChanged, false,
					postCondNumber, currentImageName.concat(buildExtension));
			postCondNumber++;
			status = false;
			LOGGER.info("####################################################################");
			LOGGER.info(
					"POST-CONDITION " + postCondNumber + ": DESCRIPTION : Reset FirmwareUpgradeStartTime to default");
			LOGGER.info("POST-CONDITION " + postCondNumber
					+ ": ACTION : Execute command: dmcli eRT setv Device.DeviceInfo.X_RDKCENTRAL-COM_MaintenanceWindow.FirmwareUpgradeStartTime with initial value or Default");
			LOGGER.info("POST-CONDITION " + postCondNumber + ": EXPECTED : Value set successfully");
			LOGGER.info("####################################################################");
			if (CommonMethods.isNotNull(maintenanceWindowStartTime)) {
				status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_COMMAND_MAINTENANCE_WINDOW_START_TIME,
						BroadBandTestConstants.CONSTANT_0, maintenanceWindowStartTime);
			} else {
				/*
				 * status =
				 * BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device,
				 * tapEnv, BroadBandWebPaConstants.WEBPA_COMMAND_MAINTENANCE_WINDOW_START_TIME,
				 * BroadBandTestConstants.CONSTANT_0,
				 * BroadBandTestConstants.DEFAULT_FIRMWARE_UPGRADE_MAINTENANCE_WINDOW_START_TIME
				 * );
				 */
			}
			if (status) {
				LOGGER.info("POST-CONDITION " + postCondNumber + ": ACTUAL : "
						+ BroadBandWebPaConstants.WEBPA_COMMAND_MAINTENANCE_WINDOW_START_TIME
						+ "value updated Successfully");
			} else {
				LOGGER.error("POST-CONDITION " + postCondNumber + ": ACTUAL : Failed to update  "
						+ BroadBandWebPaConstants.WEBPA_COMMAND_MAINTENANCE_WINDOW_START_TIME + "");
			}

			postCondNumber++;
			status = false;
			LOGGER.info("####################################################################");
			LOGGER.info("POST-CONDITION " + postCondNumber + ": DESCRIPTION : Reset FirmwareUpgradeEndTime to default");
			LOGGER.info("POST-CONDITION " + postCondNumber
					+ ": ACTION : Execute command:dmcli eRT setv Device.DeviceInfo.X_RDKCENTRAL-COM_MaintenanceWindow.FirmwareUpgradeEndTime intitial value or Default");
			LOGGER.info("POST-CONDITION " + postCondNumber + ": EXPECTED : Value set successfully");
			LOGGER.info("####################################################################");
			if (CommonMethods.isNotNull(maintenanceWindowEndTime)) {
				status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_COMMAND_MAINTENANCE_WINDOW_END_TIME,
						BroadBandTestConstants.CONSTANT_0, maintenanceWindowEndTime);
			} else {
				/*
				 * status =
				 * BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device,
				 * tapEnv, BroadBandWebPaConstants.WEBPA_COMMAND_MAINTENANCE_WINDOW_END_TIME,
				 * BroadBandTestConstants.CONSTANT_0,
				 * BroadBandTestConstants.DEFAULT_FIRMWARE_UPGRADE_MAINTENANCE_WINDOW_END_TIME);
				 */
			}
			if (status) {
				LOGGER.info("POST-CONDITION " + postCondNumber + ": ACTUAL : "
						+ BroadBandWebPaConstants.WEBPA_COMMAND_MAINTENANCE_WINDOW_END_TIME
						+ " Value set successfully");
			} else {
				LOGGER.error("POST-CONDITION " + postCondNumber + ": ACTUAL : Failed to update "
						+ BroadBandWebPaConstants.WEBPA_COMMAND_MAINTENANCE_WINDOW_END_TIME + "");
			}
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-XCONF-1001");
		LOGGER.info("#######################################################################################");
	}

	/**
	 * Trigger XCONF/DIFD HTTP CDL with same image version.
	 * 
	 * <p>
	 * STEP 1
	 * <li>Configure XCONF server with same image version using REST API</li>
	 * <li>Should be able to configure the code download configuration using XCONF
	 * Rest API and swupdate.conf should updated with conf url</li>
	 * </p>
	 * <p>
	 * STEP 2
	 * <li>execute triggerFirmwareDownload.sh</li>
	 * <li>Device should start the code download</li>
	 * </p>
	 * <p>
	 * STEP 3
	 * <li>Verify XCONF server response</li>
	 * <li>/rdklogs/logs/xconf.txt.0 file should contains the configuration in JSON
	 * Format</li>
	 * </p>
	 * <p>
	 * STEP 4
	 * <li>Verify whether download request is rejected</li>
	 * <li>Code download request should reject with following message in xconf.txt.0
	 * XCONF SCRIPT : Current image ( <current_image_name>) and Requested imgae
	 * (<current_image_name>) are same. No upgrade/downgrade required</li>
	 * </p>
	 * <p>
	 * 
	 * @param device instance of {@link Dut}
	 * 
	 * @author styles mangalasseri
	 * @Refactor Athira
	 */
	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true)
	@TestDetails(testUID = "TC-RDKB-XCONF-1003")
	public void testVerifyXconfDiffereredHttpCdlWithSameImageVersion(Dut device) {
		// execution status
		boolean status = false;
		// Test case id
		String testId = "TC-RDKB-XCONF-003";
		// variable to hold the server response
		String response = null;
		// image name to perform CDL
		String imageNameForCdl = null;
		// reboot immediately status
		String rebootImmediately = "true";
		// download protocol
		String downloadProtocol = AutomaticsConstants.FIRMWARE_DOWNLOAD_PROTOCOL_HTTP;
		// error message
		String errorMessage = null;
		// variable to hold the expected log message for validation
		String expectedLogMessage = null;
		long currentTime;
		String step = "S1";
		int stepNumber = BroadBandTestConstants.CONSTANT_1;

		boolean isCdlDataPosted = false;
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE -TC-RDKB-XCONF-1003");
		LOGGER.info("TEST DESCRIPTION: Verify whether the XCONF/DIFD HTTP CDL for the same image version");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Configure XCONF server with required configuration using REST API");
		LOGGER.info("2. Initiate cdl using webpa command");
		LOGGER.info("3. Verify XCONF server response");
		LOGGER.info("4. Verify whether download request is rejected with proper error message");
		LOGGER.info("#######################################################################################");

		/*
		 * STEP 1
		 * 
		 * Configure XCONF server with required configuration using REST API
		 */
		try {
			step = "S" + stepNumber;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION: Configure XCONF server with required configuration using REST API");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Create /nvram/swupdate.conf file and add the software update url.");
			LOGGER.info("STEP " + stepNumber
					+ "EXPECTED : Should be able to configure the code download configuration using XCONF Rest API and swupdate.conf should updated with xconf url");
			LOGGER.info("**********************************************************************************");

			try {
				// Retrieve current image name for performing CDL
				imageNameForCdl = BroadBandCommonUtils.getCurrentlyRunningImageVersionUsingWebPaCommand(tapEnv, device);
				// retry from version.txt file
				if (CommonMethods.isNull(imageNameForCdl)) {
					imageNameForCdl = CodeDownloadUtils.getCurrentRunningImageNameFromVersionTxtFile(device, tapEnv);
					LOGGER.info("Current Running ImageName From Version Txt File : " + imageNameForCdl);
				}

				if (CommonMethods.isNotNull(imageNameForCdl)) {
					BroadBandXconfCdlUtils.configureRdkbDeviceForXconfCdl(tapEnv, device, imageNameForCdl, true,
							downloadProtocol);
					status = true;
					isCdlDataPosted = status;
				} else {
					errorMessage = "Unable to retrieve current firmware file name for RDKB devices";
				}

			} catch (Exception exception) {
				status = false;
				errorMessage = exception.getMessage();
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ ": ACTUAL : Successfully configured XCONF server with required configuration using REST API");
			} else {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/*
			 * STEP 2
			 * 
			 * Trigger the XCONF CDL using WebPA Parameter - Set with value "true"
			 */

			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Initiate cdl using webpa command");
			LOGGER.info("STEP " + stepNumber + ": ACTION : EXECUTE WEBPA COMMAND to set value to true.");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : Should be able to Initiate cdl using webpa command");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Failed to initiate cdl using webpa command";

			try {
				status = BroadBandXconfCdlUtils.initiateXconfCdlThroughWebpa(tapEnv, device);
			} catch (Exception exception) {
				status = false;
				errorMessage = "Failed to initiate XCONF CDL using WebPA " + exception.getMessage();
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL : Successfully initiated XCONF CDL using WebPA ");
			} else {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/*
			 * STEP 3
			 * 
			 * Verify XCONF server response
			 */

			// reset variables
			stepNumber++;
			step = "S" + stepNumber;
			errorMessage = AutomaticsConstants.EMPTY_STRING;
			status = false;
			currentTime = System.currentTimeMillis();
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify XCONF server response");

			LOGGER.info("STEP " + stepNumber
					+ ": ACTION :  Verify XCONF SCRIPT : HTTP RESPONSE CODE is 200 in /rdklogs/logs/xconf.txt.0");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : /rdklogs/logs/xconf.txt.0 file should contains the configuration as configured");
			LOGGER.info("**********************************************************************************");

			do {
				try {
					response = tapEnv.executeCommandInSettopBox(device,
							RDKBTestConstants.CAT_COMMAND + BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0);

					if (CommonMethods.isNotNull(response)) {
						FirmwareDownloadUtils.retrieveAndValidateXconfConfiguration(response, imageNameForCdl,
								rebootImmediately, downloadProtocol);
						status = true;
					} else {
						errorMessage = "Unable to read the contents from "
								+ BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0;
						LOGGER.error(errorMessage);
					}
				} catch (Exception exception) {
					status = false;
					errorMessage = exception.getMessage();
					LOGGER.error(errorMessage);
				}
			} while (!status && (System.currentTimeMillis() - currentTime) < AutomaticsConstants.THREE_MINUTES);
			if (status) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL : Successfully executed triggerFirmwareDownload.sh");
			} else {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/*
			 * STEP 4
			 * 
			 * Verify whether download request is rejected with proper error message
			 */
			// reset variables
			stepNumber++;
			step = "S" + stepNumber;
			errorMessage = AutomaticsConstants.EMPTY_STRING;
			expectedLogMessage = AutomaticsConstants.EMPTY_STRING;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Verify whether download request is rejected with proper error message");

			LOGGER.info("STEP " + stepNumber + ": ACTION :  Verify XCONF SCRIPT RESPONSE in /rdklogs/logs/xconf.txt.0");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED: /rdklogs/logs/xconf.txt.0 file should contains the error message in the format of\" "
					+ BroadBandCdlConstants.REJECTION_MESSAGE_FOR_XCONF_HTTP_DOWNLOAD_WITH_SAME_IMAGE_IN_RDKB + "\"");
			LOGGER.info("**********************************************************************************");

			// format current image name
			imageNameForCdl = BroadBandCommonUtils.removeDifferentSignedExtensionsInRequestedBuildName(imageNameForCdl);

			response = tapEnv.executeCommandInSettopBox(device,
					RDKBTestConstants.CAT_COMMAND + BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0);
			if (CommonMethods.isNotNull(response)) {
				try {
					// verify the rejection message
					expectedLogMessage = BroadBandCdlConstants.REJECTION_MESSAGE_FOR_XCONF_HTTP_DOWNLOAD_WITH_SAME_IMAGE_IN_RDKB
							.replaceAll(BroadBandCdlConstants.CONFIGURATION_CURRENT_IMAGE_NAME, imageNameForCdl);

					// workaround to overcome the type error in RDK Code
					response = response.replaceAll("imgae", "image");

					status = response.toLowerCase().contains(expectedLogMessage.toLowerCase());

					errorMessage = "Unable to find the XCONF HTTP CDL rejection message " + expectedLogMessage + " in "
							+ BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0;
				} catch (Exception exception) {
					errorMessage = exception.getMessage();
				}
			} else {
				if (CommonMethods.isSTBAccessible(device)) {
					errorMessage = "Unable to read the contents from "
							+ BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0;
				} else {

					errorMessage = "Box is Rebooted which implies cdl is triggered";
				}
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ ": ACTUAL : Successfully validated the XCONF HTTP CDL rejection message for the same image version in "
						+ BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0);
			} else {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}

			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			tapEnv.executeCommandUsingSsh(device, "rm -rf /rdklogs/logs/xconf.txt");
			/**
			 * POST CONDITION : POST-CONDITION METHOD TO CLEAR THE CDL INFORMATION URL
			 */
			if (isCdlDataPosted) {
				BroadBandPostConditionUtils.executePostConditionToClearCdlInfoInXconf(device, tapEnv,
						BroadBandTestConstants.CONSTANT_1);
			}
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-XCONF-1003");
	}

    /**
     * Method to trigger XCONF/DIFD TFTP CDL with different image version.
     * 
     * <p>
     * STEP 1
     * <li>Configure XCONF server with required configuration using REST API</li>
     * <li>Should be able to configure the code download configuration using XCONF Rest API and swupdate.conf should
     * updated with conf url</li>
     * </p>
     * <p>
     * STEP 2
     * <li>execute triggerFirmwareDownload.sh</li>
     * <li>Device should start the code download</li>
     * </p>
     * <p>
     * STEP 3
     * <li>Verify XCONF server response</li>
     * <li>rdklogs/logs/xconf.txt.0 file should contains the configuration in JSON format</li>
     * </p>
     * <p>
     * STEP 4
     * <li>Verify whether firmware download scripts parse the xconf configuration properly</li>
     * <li>The log file should contains the download protocol</li>
     * </p>
     * <p>
     * STEP 5
     * <li>Verify whether download request is rejected with proper error message</li>
     * <li>The log file should contains the following error message, Download from tftp server not supported, check
     * XCONF server configurations</li>
     * </p>
     * 
     * @param device
     *            instance of {@link Dut}
     * 
     * @author styles mangalasseri
     * @Refactor Athira
     */

    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true)
    @TestDetails(testUID = "TC-RDKB-XCONF-1004")
    public void testVerifyXconfTftpCdl(Dut device) {
	// execution status
	boolean status = false;
	// Test case id
	String testId = "TC-RDKB-XCONF-004";
	// test step number
	String step = "s1";
	// variable to hold the server response
	String response = null;
	// String response = null;
	String currentImageName = null;
	// image name to perform CDL
	String imageNameForCdl = null;
	// reboot immediately status
	String rebootImmediately = "false";
	// download protocol
	String downloadProtocol = AutomaticsConstants.FIRMWARE_DOWNLOAD_PROTOCOL_TFTP;
	// error message
	String errorMessage = null;
	int stepNumber = 1;
	long currentTime;
	boolean isCdlDataPosted = false;
	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE TC-RDKB-XCONF-1004");
	LOGGER.info("TEST DESCRIPTION: Verify whether the XCONF/DIFD TFTP CDL for the same image version");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Configure XCONF server with required configuration using REST API");
	LOGGER.info("2. Initiate cdl using webpa command");
	LOGGER.info("3. Verify XCONF server response");
	LOGGER.info("4. Verify whether download request is rejected with proper error message");
	LOGGER.info("#######################################################################################");

	/*
	 * STEP 1
	 * 
	 * Configure XCONF server with required configuration using REST API
	 */
	try {
	    step = "S" + stepNumber;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION: Configure XCONF server with required configuration using REST API");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Create /nvram/swupdate.conf file and add the software update url.");
	    LOGGER.info("STEP " + stepNumber
		    + "EXPECTED : Should be able to configure the code download configuration using XCONF Rest API and swupdate.conf should updated with xconf url");
	    LOGGER.info("**********************************************************************************");

	    try {
		// retrieve current image name
		currentImageName = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device);
		// Retrieve image name for cdl
		if (CommonMethods.isNull(imageNameForCdl)) {
		    imageNameForCdl = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
			    BroadBandPropertyKeyConstants.PARTIAL_PROPERTY_KEY_FOR_GA_IMAGE);
		    LOGGER.info("Latest Firmware version from property file: " + imageNameForCdl);
		}

		if (CommonMethods.isNotNull(imageNameForCdl)) {
		    BroadBandXconfCdlUtils.configureRdkbDeviceForXconfCdl(tapEnv, device, imageNameForCdl, false,
			    downloadProtocol);
		    status = true;
		    isCdlDataPosted = status;
		} else {
		    errorMessage = "Unable to retrieve latest image name for RDKB devices";
		}

	    } catch (Exception exception) {
		status = false;
		errorMessage = exception.getMessage();
	    }

	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ ": ACTUAL : Successfully configured XCONF server with required configuration using REST API");
	    } else {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

	    /*
	     * STEP 2
	     * 
	     * Trigger the XCONF CDL using WebPA Parameter - Set parameter with value "true"
	     * 
	     */

	    // reset variables
	    stepNumber++;
	    step = "S" + stepNumber;
	    errorMessage = AutomaticsConstants.EMPTY_STRING;
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Initiate cdl using webpa command");
	    LOGGER.info("STEP " + stepNumber + ": ACTION : EXECUTE WEBPA COMMAND to set value to true.");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : Should be able to Initiate cdl using webpa command");
	    LOGGER.info("**********************************************************************************");

	    try {
		status = BroadBandXconfCdlUtils.initiateXconfCdlThroughWebpa(tapEnv, device);
	    } catch (Exception exception) {
		status = false;
		errorMessage = "Failed to initiate XCONF CDL using WebPA" + exception.getMessage();
	    }

	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : Successfully initiated XCONF CDL using WebPA");
	    } else {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

	    /*
	     * STEP 3
	     * 
	     * Verify XCONF server response
	     */

	    // reset variables
	    stepNumber++;
	    step = "S" + stepNumber;
	    errorMessage = AutomaticsConstants.EMPTY_STRING;
	    status = false;
	    currentTime = System.currentTimeMillis();
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify XCONF server response");

	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION :  Verify XCONF SCRIPT : HTTP RESPONSE CODE is 200 in /rdklogs/logs/xconf.txt.0");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED : /rdklogs/logs/xconf.txt.0 file should contains the configuration as configured");
	    LOGGER.info("**********************************************************************************");

	    do {
		try {
		    response = tapEnv.executeCommandInSettopBox(device,
			    RDKBTestConstants.CAT_COMMAND + BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0);

		    if (CommonMethods.isNotNull(response)) {
			FirmwareDownloadUtils.retrieveAndValidateXconfConfiguration(response, imageNameForCdl,
				rebootImmediately, downloadProtocol);
			status = true;
		    } else {
			errorMessage = "Unable to read the contents from "
				+ BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0;
			LOGGER.error(errorMessage);
		    }
		} catch (Exception exception) {
		    status = false;
		    errorMessage = exception.getMessage();
		    LOGGER.error(errorMessage);
		}
	    } while (!status && (System.currentTimeMillis() - currentTime) < AutomaticsConstants.THREE_MINUTES);

	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : Successfully executed triggerFirmwareDownload.sh");
	    } else {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

	    /*
	     * STEP 4
	     * 
	     * Verify whether download request is rejected with proper error message
	     */
	    // reset variables
	    stepNumber++;
	    step = "S" + stepNumber;
	    errorMessage = AutomaticsConstants.EMPTY_STRING;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify whether download request is rejected with proper error message");

	    LOGGER.info("STEP " + stepNumber + ": ACTION :  Verify XCONF SCRIPT RESPONSE in /rdklogs/logs/xconf.txt.0");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED: /rdklogs/logs/xconf.txt.0 file should contains the error message in the format of\" "
		    + BroadBandCdlConstants.REJECTION_MESSAGE_FOR_XCONF_TFTP_DOWNLOAD_IN_RDKB + "\"");
	    LOGGER.info("**********************************************************************************");

	    response = tapEnv.executeCommandInSettopBox(device,
		    RDKBTestConstants.CAT_COMMAND + BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0);
	    if (CommonMethods.isNotNull(response)) {
		try {
		    // added case insensitive validation since multiple devices having having different case logs
		    status = response.toLowerCase().contains(
			    BroadBandCdlConstants.REJECTION_MESSAGE_FOR_XCONF_TFTP_DOWNLOAD_IN_RDKB.toLowerCase());
		    errorMessage = "Unable to find the XCONF CDL rejection message "
			    + BroadBandCdlConstants.REJECTION_MESSAGE_FOR_XCONF_TFTP_DOWNLOAD_IN_RDKB + " in "
			    + BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0;
		} catch (Exception exception) {
		    errorMessage = exception.getMessage();
		}
	    } else {
		if (CommonMethods.isSTBAccessible(device)) {
		    errorMessage = "Unable to read the contents from "
			    + BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0;
		} else {
		    errorMessage = "Box is Rebooted which implies cdl is triggered";
		}
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ ": ACTUAL : Successfully validated the XCONF TFTP CDL is rejecting with proper error message.");
	    } else {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    tapEnv.executeCommandUsingSsh(device, "rm -rf /rdklogs/logs/xconf.txt");
	    /**
	     * POST CONDITION : POST-CONDITION METHOD TO CLEAR THE CDL INFORMATION URL
	     */
	    if (isCdlDataPosted) {
		BroadBandPostConditionUtils.executePostConditionToClearCdlInfoInXconf(device, tapEnv,
			BroadBandTestConstants.CONSTANT_1);
	    }
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-XCONF-1004");
    }
    
	/**
	 * <li>1. Update auto exclude enable as false using webpa<\li>
	 * <li>2. Update Xconf url as empty using dmcli & delete swupdate.conf file<\li>
	 * <li>3. Verify prod xconf url used as default for cdl in xconf log file<\li>
	 * <li>4. Verify http 404 response from the xconf log file<\li>
	 * 
	 * @author ArunKumar Jayachandran
	 * @refactor anandam
	 */

	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-CDL-EXCLUDE-1001")
	public void verifyAutoExcludeFromFWUpgrade1(Dut device) {
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-CDL-EXCLUDE-1001");
		LOGGER.info(
				"TEST DESCRIPTION: Test to verify fw update exclusion & url selection as exclusion is false url is empty & swupdate.conf is empty");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Update auto exclude enable as false using webpa");
		LOGGER.info("2. Update Xconf url as empty using dmcli & delete swupdate.conf file");
		LOGGER.info("3. Verify prod xconf url used as default for cdl in xconf log file");
		LOGGER.info("4. Verify http 404 response from the xconf log file");
		LOGGER.info("#######################################################################################");

		// variable declaration begins
		// Status of test script verification
		boolean status = false;
		// Test case id
		String testCaseId = "TC-RDKB-CDL-EXCLUDE-101";
		// Test step number
		String stepNumber = "s1";
		// String to store error message
		String errorMessage = null;
		// String to store response
		String response = null;
		int stepCount = BroadBandTestConstants.CONSTANT_1;
		long startTime = BroadBandTestConstants.CONSTANT_0;
		// variable declaration ends

		try {
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			/**
			 * PRE-CONDITION 1 : Pre-Condition method to disable code big first.
			 */
			BroadBandPreConditionUtils.executePreConditionToDisableCodeBigFirst(device, tapEnv,
					BroadBandTestConstants.CONSTANT_1);
			LOGGER.info("#######################################################################################");

			try {
				tapEnv.executeCommandUsingSsh(device,
						BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.CMD_ECHO,
								BroadBandTestConstants.TEXT_DOUBLE_QUOTE, BroadBandTestConstants.TEXT_DOUBLE_QUOTE,
								BroadBandTestConstants.STRING_GREATER_SYMBOL,
								BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
								BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0));
			} catch (Exception e) {
				LOGGER.info("Failed to clear xconf log file " + e.getMessage());
			}
			/* Step 1: configure auto exclude enable as false */
			stepCount = configureAutoExcludeEnableParameter(device, tapEnv, testCaseId, stepCount,
					BroadBandTestConstants.FALSE);
			/* Step 2: configure auto exclude url as empty */
			stepCount = configureAutoExcludeUrlParameter(device, tapEnv, testCaseId, ++stepCount,
					BroadBandTestConstants.EMPTY_STRING, true);

			// step 3:
			stepCount++;
			stepNumber = "s" + stepCount;
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP " + stepCount
					+ ": DESCRIPTION: Verify prod xconf url used as default for cdl in xconf log file");
			LOGGER.info("STEP " + stepCount
					+ ": ACTION: Execute command: grep -i \"Device retrieves firmware update from url=\" /rdklogs/logs/xconf.txt.0");
			LOGGER.info("STEP " + stepCount + ": EXPECTED: Should get the log message for xconf url as "
					+ BroadbandPropertyFileHandler.getProdCDLServerURL());
			LOGGER.info("******************************************************************************");
			errorMessage = "Failed to get the log message firmware update url as "
					+ BroadbandPropertyFileHandler.getProdCDLServerURL() + "  in xconf log file";
			tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.FILE_FIRMWARE_SCHED_SH);
			startTime = System.currentTimeMillis();
			do {
				tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
				response = BroadBandCommonUtils.searchLogsInArmConsoleOrConsole(device, tapEnv,
						BroadbandPropertyFileHandler.getProdCDLServerURL());
				response = CommonMethods.isNotNull(response) ? response.trim() : null;
			} while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.TWO_MINUTE_IN_MILLIS
					&& CommonMethods.isNull(response));
			status = CommonMethods.isNotNull(response) && CommonUtils.isGivenStringAvailableInCommandOutput(response,
					BroadBandTraceConstants.LOG_MESSAGE_FW_UPDATE_FROM_URL);
			if (status) {
				LOGGER.info("STEP " + stepCount
						+ ": ACTUAL: Successfully verified prod xconf url used for CDL in xconf log file");
			} else {
				LOGGER.error("STEP " + stepCount + ": ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepCount++;
			stepNumber = "s" + stepCount;
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP " + stepCount + ": DESCRIPTION: Verify http 404 response from the xconf log file");
			LOGGER.info("STEP " + stepCount
					+ ": ACTION: Execute command: grep -i \"HTTP RESPONSE CODE is 404\" /rdklogs/logs/xconf.txt.0");
			LOGGER.info("STEP " + stepCount
					+ ": EXPECTED: Should get the HTTP code as 404 and CDL should skip for prod xconf url");
			LOGGER.info("******************************************************************************");
			errorMessage = "Failed to get the log message http 404 in xconf log file";
			status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandTraceConstants.LOG_MESSAGE_HTTP_RESPONSE_CODE_404,
					BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0, BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
			CommonUtils.deleteFile(device, tapEnv, BroadBandTestConstants.SOFTWARE_UPDATE_CONF_FILE);
			if (status) {
				LOGGER.info("STEP " + stepCount
						+ ": ACTUAL: Successfully verified the log message http 404 response in xconf log file");
			} else {
				LOGGER.error("STEP " + stepCount + ": ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("Exception Occurred while Verifying auto exclude from firmware upgrade" + errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
					false);
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-CDL-EXCLUDE-1001");
		// ###############################################################//
	}

	/**
	 * <li>1. Update auto exclude enable as false using webpa<\li>
	 * <li>2. Update Xconf url as empty using dmcli & delete swupdate.conf file<\li>
	 * <li>3. Update swupdate.conf file with MOCK xcocnf url and verify triggered
	 * cdl successfull with latest firmware version through xconf<\li>
	 * 
	 * @author ArunKumar Jayachandran
	 */

	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-CDL-EXCLUDE-1002")
	public void verifyAutoExcludeFromFWUpgrade2(Dut device) {
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-CDL-EXCLUDE-1002");
		LOGGER.info(
				"TEST DESCRIPTION: Test to verify fw update exclusion & url selection as exclusion is false url is empty & swupdate.conf is MOCK XCONF URL");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Update auto exclude enable as false using webpa");
		LOGGER.info("2. Update Xconf url as empty using dmcli & delete swupdate.conf file");
		LOGGER.info(
				"3. Update swupdate.conf file with MOCK xcocnf url and verify triggered cdl successfull with latest firmware version through xconf");
		LOGGER.info("#######################################################################################");

		// variable declaration begins
		// Status of test script verification
		boolean status = false;
		// Test case id
		String testCaseId = "TC-RDKB-CDL-EXCLUDE-102";
		// Test step number
		String stepNumber = "s1";
		// String to store error message
		String errorMessage = null;
		int stepCount = BroadBandTestConstants.CONSTANT_1;
		String initialFirmwareVersion = null;
		String cdlLogsForValidation = null;
		String latestFirmwareVersion = null;
		boolean hasLatestBuildChanged = false;

		// variable declaration ends

		try {
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			/**
			 * PRE-CONDITION 1 : Pre-Condition method to disable code big first.
			 */
			BroadBandPreConditionUtils.executePreConditionToDisableCodeBigFirst(device, tapEnv,
					BroadBandTestConstants.CONSTANT_1);
			LOGGER.info("#######################################################################################");
			try {
				tapEnv.executeCommandUsingSsh(device,
						BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.CMD_ECHO,
								BroadBandTestConstants.TEXT_DOUBLE_QUOTE, BroadBandTestConstants.TEXT_DOUBLE_QUOTE,
								BroadBandTestConstants.STRING_GREATER_SYMBOL,
								BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
								BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0));
			} catch (Exception e) {
				LOGGER.info("Failed to clear xconf log file " + e.getMessage());
			}
			/* Step 1: configure auto exclude enable as false */
			stepCount = configureAutoExcludeEnableParameter(device, tapEnv, testCaseId, stepCount,
					BroadBandTestConstants.FALSE);
			/* Step 2: configure auto exclude url as empty */
			stepCount = configureAutoExcludeUrlParameter(device, tapEnv, testCaseId, ++stepCount,
					BroadBandTestConstants.EMPTY_STRING, true);

			// Step 3:
			stepNumber = "s" + ++stepCount;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepCount
					+ ": DESCRIPTION : Update swupdate.conf file with MOCK xcocnf url and verify triggered cdl successfull with latest firmware version through xconf");
			LOGGER.info("STEP " + stepCount + ": ACTION : Trigger the cdl with latest firmware version through xconf");
			LOGGER.info("STEP " + stepCount + ": EXPECTED : Cdl must successful with latest firmware version");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to trigger the image upgrade on the device with latest firmware version.";
			initialFirmwareVersion = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device);
			LOGGER.info("CURRENT FIRMWARE VERSION: " + initialFirmwareVersion);

			latestFirmwareVersion = tapEnv.getLatestBuildImageVersionForCdlTrigger(device, false);
			LOGGER.info("LATEST FIRMWARE VERSION: " + latestFirmwareVersion);
			if (CommonMethods.isNull(latestFirmwareVersion)) {
				LOGGER.info(
						" GA image obtained from deployed version service is null. Hence getting the image from property file ");
				latestFirmwareVersion = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
						BroadBandPropertyKeyConstants.PARTIAL_PROPERTY_KEY_FOR_GA_IMAGE);
				LOGGER.info("Latest Firmware version from property file: " + latestFirmwareVersion);
			}
			LOGGER.info("LATEST FIRMWARE VERSION: " + latestFirmwareVersion);
			cdlLogsForValidation = BroadBandXconfCdlUtils.triggerXconfCodeDownloadWithSwupdateOption(device, tapEnv,
					latestFirmwareVersion, true);
			errorMessage = "Failed to get cdl logs from /rdklogs/logs/xconf.txt.0";
			status = CommonMethods.isNotNull(cdlLogsForValidation) && BroadBandXconfCdlUtils.verifyCdlandReboot(device,
					tapEnv, latestFirmwareVersion, cdlLogsForValidation);
			LOGGER.info("FLASHED THE LATEST BUILD ON THE DEVICE: " + status);
			if (status) {
				hasLatestBuildChanged = status;
				LOGGER.info(
						"STEP " + stepCount + " : ACTUAL : Cdl successful with latest firmware version through xconf");
			} else {
				LOGGER.error("STEP " + stepCount + " : ACTUAL :" + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("Exception Occurred while Verifying auto exclude from firmware upgrade" + errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
					false);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			CommonUtils.deleteFile(device, tapEnv, BroadBandTestConstants.SOFTWARE_UPDATE_CONF_FILE);
			CommonUtils.deleteFile(device, tapEnv, BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0);
			if (hasLatestBuildChanged) {
				/**
				 * POST CONDITION : VERIFY THAT DEVICE IS UPGRADED WITH THE PREVIOUS FIRMWARE
				 * VERSION.
				 */
				BroadBandPostConditionUtils.executePostConditionToTriggerCdl(device, tapEnv, true, false,
						BroadBandTestConstants.CONSTANT_1, initialFirmwareVersion);
			}
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-CDL-EXCLUDE-1002");
		// ###############################################################//
	}

	/**
	 * <li>1. Update auto exclude enable as false using webpa<\li>
	 * <li>2. Update Xconf url as CI XCONF URL using webpa & delete swupdate.conf
	 * file<\li>
	 * <li>3. Update swupdate.conf file with MOCK xcocnf url and verify triggered
	 * cdl successfull with latest firmware version through xconf<\li>
	 * 
	 * @author ArunKumar Jayachandran
	 */

	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-CDL-EXCLUDE-1003")
	public void verifyAutoExcludeFromFWUpgrade3(Dut device) {
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-CDL-EXCLUDE-1003");
		LOGGER.info(
				"TEST DESCRIPTION: Test to verify fw update exclusion & url selection as exclusion is false url is CI XCONF URL & swupdate.conf is MOCK XCONF URL");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Update auto exclude enable as false using webpa");
		LOGGER.info("2. Update Xconf url as CI XCONF URL using webpa & delete swupdate.conf file");
		LOGGER.info(
				"3. Update swupdate.conf file with MOCK xcocnf url and verify triggered cdl successfull with latest firmware version through xconf");
		LOGGER.info("#######################################################################################");

		// variable declaration begins
		// Status of test script verification
		boolean status = false;
		// Test case id
		String testCaseId = "TC-RDKB-CDL-EXCLUDE-103";
		// Test step number
		String stepNumber = "s1";
		// String to store error message
		String errorMessage = null;
		int stepCount = BroadBandTestConstants.CONSTANT_1;
		String initialFirmwareVersion = null;
		String cdlLogsForValidation = null;
		String latestFirmwareVersion = null;
		boolean hasLatestBuildChanged = false;
		String ciXconfUrl = BroadbandPropertyFileHandler.getCIServerSwupdateURL();
		// variable declaration ends

		try {
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			/**
			 * PRE-CONDITION 1 : Pre-Condition method to disable code big first.
			 */
			BroadBandPreConditionUtils.executePreConditionToDisableCodeBigFirst(device, tapEnv,
					BroadBandTestConstants.CONSTANT_1);
			LOGGER.info("#######################################################################################");
			try {
				tapEnv.executeCommandUsingSsh(device,
						BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.CMD_ECHO,
								BroadBandTestConstants.TEXT_DOUBLE_QUOTE, BroadBandTestConstants.TEXT_DOUBLE_QUOTE,
								BroadBandTestConstants.STRING_GREATER_SYMBOL,
								BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
								BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0));
			} catch (Exception e) {
				LOGGER.info("Failed to clear xconf log file " + e.getMessage());
			}

			/* Step 1: configure auto exclude enable as false */
			stepCount = configureAutoExcludeEnableParameter(device, tapEnv, testCaseId, stepCount,
					BroadBandTestConstants.FALSE);
			/* Step 2: configure auto exclude url as empty */
			stepCount = configureAutoExcludeUrlParameter(device, tapEnv, testCaseId, ++stepCount, ciXconfUrl, true);

			// Step 3:
			stepNumber = "s" + ++stepCount;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepCount
					+ ": DESCRIPTION : Update swupdate.conf file with MOCK xcocnf url and verify triggered cdl successfull with latest firmware version through xconf");
			LOGGER.info("STEP " + stepCount + ": ACTION : Trigger the cdl with latest firmware version through xconf");
			LOGGER.info("STEP " + stepCount + ": EXPECTED : Cdl must successful with latest firmware version");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to trigger the image upgrade on the device with latest firmware version.";
			initialFirmwareVersion = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device);
			LOGGER.info("CURRENT FIRMWARE VERSION: " + initialFirmwareVersion);
			latestFirmwareVersion = tapEnv.getLatestBuildImageVersionForCdlTrigger(device, false);
			LOGGER.info("LATEST FIRMWARE VERSION: " + latestFirmwareVersion);
			if (CommonMethods.isNull(latestFirmwareVersion)) {
				LOGGER.info(
						" GA image obtained from deployed version service is null. Hence getting the image from property file ");
				latestFirmwareVersion = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
						BroadBandPropertyKeyConstants.PARTIAL_PROPERTY_KEY_FOR_GA_IMAGE);
				LOGGER.info("Latest Firmware version from property file: " + latestFirmwareVersion);
			}
			LOGGER.info("LATEST FIRMWARE VERSION: " + latestFirmwareVersion);
			cdlLogsForValidation = BroadBandXconfCdlUtils.triggerXconfCodeDownloadWithSwupdateOption(device, tapEnv,
					latestFirmwareVersion, true);
			errorMessage = "Failed to get cdl logs from /rdklogs/logs/xconf.txt.0";
			status = CommonMethods.isNotNull(cdlLogsForValidation) && BroadBandXconfCdlUtils.verifyCdlandReboot(device,
					tapEnv, latestFirmwareVersion, cdlLogsForValidation);
			LOGGER.info("FLASHED THE LATEST BUILD ON THE DEVICE: " + status);
			if (status) {
				hasLatestBuildChanged = status;
				LOGGER.info(
						"STEP " + stepCount + " : ACTUAL : Cdl successful with latest firmware version through xconf");
			} else {
				LOGGER.error("STEP " + stepCount + " : ACTUAL :" + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("Exception Occurred while Verifying auto exclude from firmware upgrade" + errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
					false);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			CommonUtils.deleteFile(device, tapEnv, BroadBandTestConstants.SOFTWARE_UPDATE_CONF_FILE);
			CommonUtils.deleteFile(device, tapEnv, BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0);
			if (hasLatestBuildChanged) {
				/**
				 * POST CONDITION : VERIFY THAT DEVICE IS UPGRADED WITH THE PREVIOUS FIRMWARE
				 * VERSION.
				 */
				BroadBandPostConditionUtils.executePostConditionToTriggerCdl(device, tapEnv, true, false,
						BroadBandTestConstants.CONSTANT_1, initialFirmwareVersion);
			}
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-CDL-EXCLUDE-1003");
		// ###############################################################//
	}

	/**
	 * <li>1. Update auto exclude enable as false using webpa<\li>
	 * <li>2. Update Xconf url as MOCK XCONF URL using webpa & delete swupdate.conf
	 * file<\li>
	 * <li>3. Verify triggered cdl successfull with latest firmware version through
	 * xconf without swupdate.conf file<\li>
	 * 
	 * @author ArunKumar Jayachandran
	 * @refactor Govardhan
	 */

	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-CDL-EXCLUDE-1004")
	public void verifyAutoExcludeFromFWUpgrade4(Dut device) {
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-CDL-EXCLUDE-1004");
		LOGGER.info(
				"TEST DESCRIPTION: Test to verify fw update exclusion & url selection as exclusion is false url is MOCK XCONF URL & swupdate.conf is empty");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Update auto exclude enable as false using webpa");
		LOGGER.info("2. Update Xconf url as MOCK XCONF URL using webpa & delete swupdate.conf file");
		LOGGER.info(
				"3. Verify triggered cdl successfull with latest firmware version through xconf without swupdate.conf file");
		LOGGER.info("#######################################################################################");

		// variable declaration begins
		// Status of test script verification
		boolean status = false;
		// Test case id
		String testCaseId = "TC-RDKB-CDL-EXCLUDE-104";
		// Test step number
		String stepNumber = "s1";
		// String to store error message
		String errorMessage = null;
		// variable declaration ends
		int stepCount = BroadBandTestConstants.CONSTANT_1;
		String mockXconfUrl = XConfUtils.getXconfServerUrl(device);
		String initialFirmwareVersion = null;
		String cdlLogsForValidation = null;
		String latestFirmwareVersion = null;
		boolean hasLatestBuildChanged = false;
		// variable declaration ends

		try {
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			/**
			 * PRE-CONDITION 1 : Pre-Condition method to disable code big first.
			 */
			BroadBandPreConditionUtils.executePreConditionToDisableCodeBigFirst(device, tapEnv,
					BroadBandTestConstants.CONSTANT_1);
			LOGGER.info("#######################################################################################");
			try {
				tapEnv.executeCommandUsingSsh(device,
						BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.CMD_ECHO,
								BroadBandTestConstants.TEXT_DOUBLE_QUOTE, BroadBandTestConstants.TEXT_DOUBLE_QUOTE,
								BroadBandTestConstants.STRING_GREATER_SYMBOL,
								BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
								BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0));
			} catch (Exception e) {
				LOGGER.info("Failed to clear xconf log file " + e.getMessage());
			}
			/* Step 1: configure auto exclude enable as false */
			stepCount = configureAutoExcludeEnableParameter(device, tapEnv, testCaseId, stepCount,
					BroadBandTestConstants.FALSE);
			/* Step 2: configure auto exclude url as empty */
			stepCount = configureAutoExcludeUrlParameter(device, tapEnv, testCaseId, ++stepCount, mockXconfUrl, true);

			// Step 3:
			stepNumber = "s" + ++stepCount;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepCount
					+ ": DESCRIPTION : Verify triggered cdl successfull with latest firmware version through xconf");
			LOGGER.info("STEP " + stepCount + ": ACTION : Trigger the cdl with latest firmware version through xconf");
			LOGGER.info("STEP " + stepCount + ": EXPECTED : Cdl must successful with latest firmware version");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to trigger the image upgrade on the device with latest firmware version.";
			initialFirmwareVersion = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device);
			LOGGER.info("CURRENT FIRMWARE VERSION: " + initialFirmwareVersion);
			latestFirmwareVersion = tapEnv.getLatestBuildImageVersionForCdlTrigger(device, false);
			LOGGER.info("LATEST FIRMWARE VERSION: " + latestFirmwareVersion);
			if (CommonMethods.isNull(latestFirmwareVersion)) {
				LOGGER.info(
						" GA image obtained from deployed version service is null. Hence getting the image from property file ");
				latestFirmwareVersion = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
						BroadBandPropertyKeyConstants.PARTIAL_PROPERTY_KEY_FOR_GA_IMAGE);
				LOGGER.info("Latest Firmware version from property file: " + latestFirmwareVersion);
			}
			LOGGER.info("LATEST FIRMWARE VERSION: " + latestFirmwareVersion);
			cdlLogsForValidation = BroadBandXconfCdlUtils.triggerXconfCodeDownloadWithSwupdateOption(device, tapEnv,
					latestFirmwareVersion, false);
			errorMessage = "Failed to get cdl logs from /rdklogs/logs/xconf.txt.0";
			status = CommonMethods.isNotNull(cdlLogsForValidation) && BroadBandXconfCdlUtils.verifyCdlandReboot(device,
					tapEnv, latestFirmwareVersion, cdlLogsForValidation);
			LOGGER.info("FLASHED THE LATEST BUILD ON THE DEVICE: " + status);
			if (status) {
				hasLatestBuildChanged = status;
				LOGGER.info(
						"STEP " + stepCount + " : ACTUAL : Cdl successful with latest firmware version through xconf");
			} else {
				LOGGER.error("STEP " + stepCount + " : ACTUAL :" + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("Exception Occurred while Verifying auto exclude from firmware upgrade" + errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
					false);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			CommonUtils.deleteFile(device, tapEnv, BroadBandTestConstants.SOFTWARE_UPDATE_CONF_FILE);
			CommonUtils.deleteFile(device, tapEnv, BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0);
			if (hasLatestBuildChanged) {
				/**
				 * POST CONDITION : VERIFY THAT DEVICE IS UPGRADED WITH THE PREVIOUS FIRMWARE
				 * VERSION.
				 */
				/**
				 * POST CONDITION : VERIFY THAT DEVICE IS UPGRADED WITH THE PREVIOUS FIRMWARE VERSION.
				 */		
				BroadBandCodeDownloadUtils.triggerPreviousCodeDownload(device, tapEnv,
					initialFirmwareVersion);
			}
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-CDL-EXCLUDE-1004");
		// ###############################################################//
	}

	/**
	 * <li>1. Update auto exclude enable as true using RFC<\li>
	 * <li>2. Update Xconf url as empty using dmcli & delete swupdate.conf file<\li>
	 * <li>3. Verify device is excluded from FW upgrade for triggered CDL<\li>
	 * <li>4. verify xconf file is empty<\li>
	 * 
	 * @author ajayac200
	 */

	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-CDL-EXCLUDE-1005")
	public void verifyAutoExcludeFromFWUpgrade5(Dut device) {
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-CDL-EXCLUDE-1005");
		LOGGER.info(
				"TEST DESCRIPTION: Test to verify fw update exclusion & url selection as exclusion is true url is empty & swupdate.conf is empty");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Update auto exclude enable as true using RFC");
		LOGGER.info("2. Update Xconf url as empty using dmcli & delete swupdate.conf file");
		LOGGER.info("3. Verify device is excluded from FW upgrade for triggered CDL");
		LOGGER.info("4. verify xconf file is empty");
		LOGGER.info("#######################################################################################");

		// variable declaration begins
		// Status of test script verification
		boolean status = false;
		// Test case id
		String testCaseId = "TC-RDKB-CDL-EXCLUDE-105";
		// Test step number
		String stepNumber = "s1";
		// String to store error message
		String errorMessage = null;
		// String to store response
		String response = null;
		int stepCount = BroadBandTestConstants.CONSTANT_1;
		// variable declaration ends

		try {
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			/**
			 * PRE-CONDITION 1 : Pre-Condition method to disable code big first.
			 */
			BroadBandPreConditionUtils.executePreConditionToDisableCodeBigFirst(device, tapEnv,
					BroadBandTestConstants.CONSTANT_1);
			LOGGER.info("#######################################################################################");
			try {
				tapEnv.executeCommandUsingSsh(device,
						BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.CMD_ECHO,
								BroadBandTestConstants.TEXT_DOUBLE_QUOTE, BroadBandTestConstants.TEXT_DOUBLE_QUOTE,
								BroadBandTestConstants.STRING_GREATER_SYMBOL,
								BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
								BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0));
			} catch (Exception e) {
				LOGGER.info("Failed to clear xconf log file " + e.getMessage());
			}
			/* Step 1: configure auto exclude enable as true */
			stepNumber = "s" + stepCount;
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP " + stepCount + ": DESCRIPTION: Update auto exclude enable as true using RFC");
			LOGGER.info("STEP " + stepCount
					+ ": ACTION: Execute command: Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.FWUpdate.AutoExcluded.Enable as true");
			LOGGER.info("STEP " + stepCount + ": EXPECTED: Should update the AutoExclude.Enable parameter as true");
			LOGGER.info("******************************************************************************");
			errorMessage = "Failed to update the AutoExclude.Enable parameter as true using RFC";
			status = BroadBandRfcFeatureControlUtils.enableOrDisableFeatureByRFC(tapEnv, device,
					BroadBandTestConstants.FEATURE_NAME_AUTO_EXCLUDE, true);
			if (status) {
				LOGGER.info(
						"STEP " + stepCount + ": ACTUAL: Successfully updated AutoExclude.Enable as true using RFC");
			} else {
				LOGGER.error("STEP " + stepCount + ": ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			/* Step 2: configure auto exclude url as empty */
			stepCount = configureAutoExcludeUrlParameter(device, tapEnv, testCaseId, ++stepCount,
					BroadBandTestConstants.EMPTY_STRING, true);

			// step 3:
			stepNumber = "s" + ++stepCount;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepCount
					+ ": DESCRIPTION : Verify device is excluded from FW upgrade for triggered CDL");
			LOGGER.info("STEP " + stepCount
					+ ": ACTION : Trigger the cdl and verify the excluded log message in Console log");
			LOGGER.info("STEP " + stepCount + ": EXPECTED : Device should log excluded from FW upgrade message");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to trigger the image upgrade on the device with latest firmware version.";
			tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.FILE_FIRMWARE_SCHED_SH);
			long startTime = System.currentTimeMillis();
			do {
				tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
				response = BroadBandCommonUtils.searchLogsInArmConsoleOrConsole(device, tapEnv,
						BroadBandTraceConstants.LOG_MESSAGE_EXCLUDED_FROM_FW_UPGRADE);
				response = CommonMethods.isNotNull(response) ? response.trim() : null;
			} while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.THREE_MINUTE_IN_MILLIS
					&& CommonMethods.isNull(response));
			status = CommonMethods.isNotNull(response);
			if (status) {
				LOGGER.info("STEP " + stepCount + " : ACTUAL : Cdl not initiated as auto xconf enable is false");
			} else {
				LOGGER.error("STEP " + stepCount + " : ACTUAL :" + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

			// step 4:
			stepCount++;
			stepNumber = "s" + stepCount;
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP " + stepCount + ": DESCRIPTION: verify xconf file is empty");
			LOGGER.info("STEP " + stepCount + ": ACTION: Execute command: cat /rdklogs/logs/xconf.txt.0");
			LOGGER.info("STEP " + stepCount + ": EXPECTED: xconf log file should be empty");
			LOGGER.info("******************************************************************************");
			errorMessage = "Firmware download started";
			response = tapEnv.executeCommandUsingSsh(device,
					BroadBandTestConstants.CAT_COMMAND + BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0);
			status = CommonMethods.isNull(response)
					|| !CommonMethods.isFileExists(device, tapEnv, BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0);
			if (status) {
				LOGGER.info("STEP " + stepCount + ": ACTUAL: Successfully verified xconf log file is empty");
			} else {
				LOGGER.error("STEP " + stepCount + ": ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("Exception Occurred while Verifying auto exclude from firmware upgrade" + errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
					false);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS");
			LOGGER.info("POST-CONDITION: DESCRIPTION: Update auto exclude enable as false using webpa");
			LOGGER.info(
					"POST-CONDITION: ACTION: Execute webpa command: Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.FWUpdate.AutoExcluded.Enable as true");
			LOGGER.info(
					"POST-CONDITION: EXPECTED: Webpa set operation should success and auto excluded should be enabled");
			status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_FWUPDATE_AUTO_EXCLUDED_ENABLE,
					BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.FALSE,
					BroadBandTestConstants.ONE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			status = (HttpStatus.SC_OK == CommonMethods.clearParamsInServer(device, tapEnv, false,
					BroadBandTestConstants.FEATURE_NAME_AUTO_EXCLUDE));
			LOGGER.info("POST-CONDITION: ACTUAL: Disabled firmware exclusion status as false");
			LOGGER.info("POST-CONFIGURATIONS: FINAL STATUS - " + status);
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-CDL-EXCLUDE-1005");
		// ###############################################################//
	}

	/**
	 * <li>1. Update auto exclude enable as true using webpa<\li>
	 * <li>2. Update Xconf url as empty using dmcli & delete swupdate.conf file<\li>
	 * <li>3. Update swupdate.conf file with MOCK url & Verify triggered cdl
	 * successfull with latest firmware version through xconf<\li>
	 * 
	 * @author ArunKumar Jayachandran
	 */

	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-CDL-EXCLUDE-1006")
	public void verifyAutoExcludeFromFWUpgrade6(Dut device) {
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-CDL-EXCLUDE-1006");
		LOGGER.info(
				"TEST DESCRIPTION: Test to verify fw update exclusion & url selection as exclusion is true url is empty & swupdate.conf is MOCK XCONF URL");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Update auto exclude enable as true using webpa");
		LOGGER.info("2. Update Xconf url as empty using dmcli & delete swupdate.conf file");
		LOGGER.info(
				"3. Update swupdate.conf file with MOCK url & Verify triggered cdl successfull with latest firmware version through xconf");
		LOGGER.info("#######################################################################################");

		// variable declaration begins
		// Status of test script verification
		boolean status = false;
		// Test case id
		String testCaseId = "TC-RDKB-CDL-EXCLUDE-106";
		// Test step number
		String stepNumber = "s1";
		// String to store error message
		String errorMessage = null;
		int stepCount = BroadBandTestConstants.CONSTANT_1;
		String cdlLogsForValidation = null;
		String initialFirmwareVersion = null;
		String latestFirmwareVersion = null;
		boolean hasLatestBuildChanged = false;
		// variable declaration ends

		try {
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			/**
			 * PRE-CONDITION 1 : Pre-Condition method to disable code big first.
			 */
			BroadBandPreConditionUtils.executePreConditionToDisableCodeBigFirst(device, tapEnv,
					BroadBandTestConstants.CONSTANT_1);
			try {
				tapEnv.executeCommandUsingSsh(device,
						BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.CMD_ECHO,
								BroadBandTestConstants.TEXT_DOUBLE_QUOTE, BroadBandTestConstants.TEXT_DOUBLE_QUOTE,
								BroadBandTestConstants.STRING_GREATER_SYMBOL,
								BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
								BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0));
			} catch (Exception e) {
				LOGGER.info("Failed to clear xconf log file " + e.getMessage());
			}
			LOGGER.info("#######################################################################################");

			/* Step 1: configure auto exclude enable as true */
			stepCount = configureAutoExcludeEnableParameter(device, tapEnv, testCaseId, stepCount,
					BroadBandTestConstants.TRUE);

			/* Step 2: configure auto exclude url as empty */
			stepCount = configureAutoExcludeUrlParameter(device, tapEnv, testCaseId, ++stepCount,
					BroadBandTestConstants.EMPTY_STRING, true);

			stepNumber = "s" + ++stepCount;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepCount
					+ ": DESCRIPTION : Update swupdate.conf file with MOCK url & Verify triggered cdl successfull with latest firmware version through xconf");
			LOGGER.info("STEP " + stepCount + ": ACTION : Trigger the cdl with latest firmware version through xconf");
			LOGGER.info("STEP " + stepCount + ": EXPECTED : Cdl must successful with latest firmware version");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to trigger the image upgrade on the device with latest firmware version.";
			initialFirmwareVersion = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device);
			LOGGER.info("CURRENT FIRMWARE VERSION: " + initialFirmwareVersion);
			latestFirmwareVersion = tapEnv.getLatestBuildImageVersionForCdlTrigger(device, false);
			LOGGER.info("LATEST FIRMWARE VERSION: " + latestFirmwareVersion);
			if (CommonMethods.isNull(latestFirmwareVersion)) {
				LOGGER.info(
						" GA image obtained from deployed version service is null. Hence getting the image from property file ");
				latestFirmwareVersion = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
						BroadBandPropertyKeyConstants.PARTIAL_PROPERTY_KEY_FOR_GA_IMAGE);
				LOGGER.info("Latest Firmware version from property file: " + latestFirmwareVersion);
			}
			LOGGER.info("LATEST FIRMWARE VERSION: " + latestFirmwareVersion);
			cdlLogsForValidation = BroadBandXconfCdlUtils.triggerXconfCodeDownloadWithSwupdateOption(device, tapEnv,
					latestFirmwareVersion, true);
			errorMessage = "Failed to get cdl logs from /rdklogs/logs/xconf.txt.0";
			status = CommonMethods.isNotNull(cdlLogsForValidation) && BroadBandXconfCdlUtils.verifyCdlandReboot(device,
					tapEnv, latestFirmwareVersion, cdlLogsForValidation);
			LOGGER.info("FLASHED THE LATEST BUILD ON THE DEVICE: " + status);
			if (status) {
				hasLatestBuildChanged = status;
				LOGGER.info(
						"STEP " + stepCount + " : ACTUAL : Cdl successful with latest firmware version through xconf");
			} else {
				LOGGER.error("STEP " + stepCount + " : ACTUAL :" + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("Exception Occurred while Verifying auto exclude from firmware upgrade" + errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
					false);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			if (hasLatestBuildChanged) {
				/**
				 * POST CONDITION : VERIFY THAT DEVICE IS UPGRADED WITH THE PREVIOUS FIRMWARE
				 * VERSION.
				 */
				BroadBandPostConditionUtils.executePostConditionToTriggerCdl(device, tapEnv, true, false,
						BroadBandTestConstants.CONSTANT_1, initialFirmwareVersion);
			}
			BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_FWUPDATE_AUTO_EXCLUDED_ENABLE,
					BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.FALSE,
					BroadBandTestConstants.ONE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			CommonUtils.deleteFile(device, tapEnv, BroadBandTestConstants.SOFTWARE_UPDATE_CONF_FILE);
			CommonUtils.deleteFile(device, tapEnv, BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0);
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");

		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-CDL-EXCLUDE-1006");
		// ###############################################################//
	}

	/**
	 * <li>1. Update auto exclude enable as true using webpa<\li>
	 * <li>2. Update Xconf url as CI XCONF URL using webpa & delete swupdate.conf
	 * file<\li>
	 * <li>3. Update swupdate.conf file with MOCK url & Verify triggered cdl
	 * successfull with latest firmware version through xconf<\li>
	 * 
	 * @author ajayac200
	 */

	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-CDL-EXCLUDE-1007")
	public void verifyAutoExcludeFromFWUpgrade7(Dut device) {
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-CDL-EXCLUDE-1007");
		LOGGER.info(
				"TEST DESCRIPTION: Test to verify fw update exclusion & url selection as exclusion is true url is CI XCONF URL & swupdate.conf is MOCK XCONF URL");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Update auto exclude enable as true using webpa");
		LOGGER.info("2. Update Xconf url as CI XCONF URL using webpa & delete swupdate.conf file");
		LOGGER.info(
				"3. Update swupdate.conf file with MOCK url & Verify triggered cdl successfull with latest firmware version through xconf");
		LOGGER.info("#######################################################################################");

		// variable declaration begins
		// Status of test script verification
		boolean status = false;
		// Test case id
		String testCaseId = "TC-RDKB-CDL-EXCLUDE-107";
		// Test step number
		String stepNumber = "s1";
		// String to store error message
		String errorMessage = null;
		int stepCount = BroadBandTestConstants.CONSTANT_1;
		String cdlLogsForValidation = null;
		String initialFirmwareVersion = null;
		String latestFirmwareVersion = null;
		boolean hasLatestBuildChanged = false;
		String ciXconfUrl = BroadbandPropertyFileHandler.getCIServerSwupdateURL();
		// variable declaration ends

		try {
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			/**
			 * PRE-CONDITION 1 : Pre-Condition method to disable code big first.
			 */
			BroadBandPreConditionUtils.executePreConditionToDisableCodeBigFirst(device, tapEnv,
					BroadBandTestConstants.CONSTANT_1);
			LOGGER.info("#######################################################################################");

			try {
				tapEnv.executeCommandUsingSsh(device,
						BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.CMD_ECHO,
								BroadBandTestConstants.TEXT_DOUBLE_QUOTE, BroadBandTestConstants.TEXT_DOUBLE_QUOTE,
								BroadBandTestConstants.STRING_GREATER_SYMBOL,
								BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
								BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0));
			} catch (Exception e) {
				LOGGER.info("Failed to clear xconf log file " + e.getMessage());
			}
			/* Step 1: configure auto exclude enable as false */
			stepCount = configureAutoExcludeEnableParameter(device, tapEnv, testCaseId, stepCount,
					BroadBandTestConstants.TRUE);

			/* Step 2: configure auto exclude url as empty */
			stepCount = configureAutoExcludeUrlParameter(device, tapEnv, testCaseId, ++stepCount, ciXconfUrl, true);

			stepNumber = "s" + ++stepCount;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepCount
					+ ": DESCRIPTION : Update swupdate.conf file with MOCK url & Verify triggered cdl successfull with latest firmware version through xconf");
			LOGGER.info("STEP " + stepCount + ": ACTION : Trigger the cdl with latest firmware version through xconf");
			LOGGER.info("STEP " + stepCount + ": EXPECTED : Cdl must successful with latest firmware version");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to trigger the image upgrade on the device with latest firmware version.";
			initialFirmwareVersion = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device);
			LOGGER.info("CURRENT FIRMWARE VERSION: " + initialFirmwareVersion);

			latestFirmwareVersion = tapEnv.getLatestBuildImageVersionForCdlTrigger(device, false);
			LOGGER.info("LATEST FIRMWARE VERSION: " + latestFirmwareVersion);
			if (CommonMethods.isNull(latestFirmwareVersion)) {
				LOGGER.info(
						" GA image obtained from deployed version service is null. Hence getting the image from property file ");
				latestFirmwareVersion = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
						BroadBandPropertyKeyConstants.PARTIAL_PROPERTY_KEY_FOR_GA_IMAGE);
				LOGGER.info("Latest Firmware version from property file: " + latestFirmwareVersion);
			}
			LOGGER.info("LATEST FIRMWARE VERSION: " + latestFirmwareVersion);
			cdlLogsForValidation = BroadBandXconfCdlUtils.triggerXconfCodeDownloadWithSwupdateOption(device, tapEnv,
					latestFirmwareVersion, true);
			errorMessage = "Failed to get cdl logs from /rdklogs/logs/xconf.txt.0";
			status = CommonMethods.isNotNull(cdlLogsForValidation) && BroadBandXconfCdlUtils.verifyCdlandReboot(device,
					tapEnv, latestFirmwareVersion, cdlLogsForValidation);
			LOGGER.info("FLASHED THE LATEST BUILD ON THE DEVICE: " + status);
			if (status) {
				hasLatestBuildChanged = status;
				LOGGER.info(
						"STEP " + stepCount + " : ACTUAL : Cdl successful with latest firmware version through xconf");
			} else {
				LOGGER.error("STEP " + stepCount + " : ACTUAL :" + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("Exception Occurred while Verifying auto exclude from firmware upgrade" + errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
					false);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			if (hasLatestBuildChanged) {
				/**
				 * POST CONDITION : VERIFY THAT DEVICE IS UPGRADED WITH THE PREVIOUS FIRMWARE
				 * VERSION.
				 */
				BroadBandPostConditionUtils.executePostConditionToTriggerCdl(device, tapEnv, true, false,
						BroadBandTestConstants.CONSTANT_1, initialFirmwareVersion);
			}
			CommonUtils.deleteFile(device, tapEnv, BroadBandTestConstants.SOFTWARE_UPDATE_CONF_FILE);
			CommonUtils.deleteFile(device, tapEnv, BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0);
			BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_FWUPDATE_AUTO_EXCLUDED_ENABLE,
					BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.FALSE,
					BroadBandTestConstants.ONE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");

		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-CDL-EXCLUDE-1007");
		// ###############################################################//
	}

	/**
	 * <li>1. Update auto exclude enable as true using webpa<\li>
	 * <li>2. Update Xconf url as MOCK XCONF URL using webpa & delete swupdate.conf
	 * file<\li>
	 * <li>3. Verify device is excluded from FW upgrade for triggered CDL<\li>
	 * <li>4. verify xconf file is empty<\li>
	 * 
	 * @author ajayac200
	 */

	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-CDL-EXCLUDE-1008")
	public void verifyAutoExcludeFromFWUpgrade8(Dut device) {
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-CDL-EXCLUDE-1008");
		LOGGER.info(
				"TEST DESCRIPTION: Test to verify fw update exclusion & url selection as exclusion is true url is MOCK XCONF URL & swupdate.conf is empty");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Update auto exclude enable as true using webpa");
		LOGGER.info("2. Update Xconf url as MOCK XCONF URL using webpa & delete swupdate.conf file");
		LOGGER.info("3. Verify device is excluded from FW upgrade for triggered CDL");
		LOGGER.info("4. verify xconf file is empty");
		LOGGER.info("#######################################################################################");

		// variable declaration begins
		// Status of test script verification
		boolean status = false;
		// Test case id
		String testCaseId = "TC-RDKB-CDL-EXCLUDE-108";
		// Test step number
		String stepNumber = "s1";
		// String to store error message
		String errorMessage = null;
		String response = null;
		int stepCount = BroadBandTestConstants.CONSTANT_1;
		String mockXconfUrl = XConfUtils.getXconfServerUrl(device);
		// variable declaration ends

		try {
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			/**
			 * PRE-CONDITION 1 : Pre-Condition method to disable code big first.
			 */
			BroadBandPreConditionUtils.executePreConditionToDisableCodeBigFirst(device, tapEnv,
					BroadBandTestConstants.CONSTANT_1);
			LOGGER.info("#######################################################################################");

			try {
				tapEnv.executeCommandUsingSsh(device,
						BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.CMD_ECHO,
								BroadBandTestConstants.TEXT_DOUBLE_QUOTE, BroadBandTestConstants.TEXT_DOUBLE_QUOTE,
								BroadBandTestConstants.STRING_GREATER_SYMBOL,
								BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
								BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0));
			} catch (Exception e) {
				LOGGER.info("Failed to clear xconf log file " + e.getMessage());
			}
			/* Step 1: configure auto exclude enable as true */
			stepCount = configureAutoExcludeEnableParameter(device, tapEnv, testCaseId, stepCount,
					BroadBandTestConstants.TRUE);

			/* Step 2: configure auto exclude url as mockXconfUrl */
			stepCount = configureAutoExcludeUrlParameter(device, tapEnv, testCaseId, ++stepCount, mockXconfUrl, true);

			// step 3:
			stepNumber = "s" + ++stepCount;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepCount
					+ ": DESCRIPTION : Verify device is excluded from FW upgrade for triggered CDL");
			LOGGER.info("STEP " + stepCount
					+ ": ACTION : Trigger the cdl and verify the excluded log message in Console log");
			LOGGER.info("STEP " + stepCount + ": EXPECTED : Device should log excluded from FW upgrade message");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to trigger the image upgrade on the device with latest firmware version.";
			tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.FILE_FIRMWARE_SCHED_SH);
			long startTime = System.currentTimeMillis();
			do {
				tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
				response = BroadBandCommonUtils.searchLogsInArmConsoleOrConsole(device, tapEnv,
						BroadBandTraceConstants.LOG_MESSAGE_EXCLUDED_FROM_FW_UPGRADE);
				response = CommonMethods.isNotNull(response) ? response.trim() : null;
			} while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.THREE_MINUTE_IN_MILLIS
					&& CommonMethods.isNull(response));
			status = CommonMethods.isNotNull(response);
			if (status) {
				LOGGER.info("STEP " + stepCount + " : ACTUAL : Cdl not initiated as auto xconf enable is false");
			} else {
				LOGGER.error("STEP " + stepCount + " : ACTUAL :" + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

			// step 4:
			stepCount++;
			stepNumber = "s" + stepCount;
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP " + stepCount + ": DESCRIPTION: verify xconf file is empty");
			LOGGER.info("STEP " + stepCount + ": ACTION: Execute command: cat /rdklogs/logs/xconf.txt.0");
			LOGGER.info("STEP " + stepCount + ": EXPECTED: xconf log file should be empty");
			LOGGER.info("******************************************************************************");
			errorMessage = "Firmware download started";
			response = tapEnv.executeCommandUsingSsh(device,
					BroadBandTestConstants.CAT_COMMAND + BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0);
			status = CommonMethods.isNull(response)
					|| !CommonMethods.isFileExists(device, tapEnv, BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0);
			if (status) {
				LOGGER.info("STEP " + stepCount + ": ACTUAL: Successfully verified xconf log file is empty");
			} else {
				LOGGER.error("STEP " + stepCount + ": ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("Exception Occurred while Verifying auto exclude from firmware upgrade" + errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
					false);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_FWUPDATE_AUTO_EXCLUDED_ENABLE,
					BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.FALSE,
					BroadBandTestConstants.ONE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");

		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-CDL-EXCLUDE-1008");
		// ###############################################################//
	}

	public static int configureAutoExcludeEnableParameter(Dut device, AutomaticsTapApi tapEnv, String testCaseId,
			int stepCount, String flag) {
		String stepNumber = null;
		boolean status = false;
		String errorMessage = null;

		stepNumber = "s" + stepCount;
		status = false;
		LOGGER.info("******************************************************************************");
		LOGGER.info("STEP " + stepCount + ": DESCRIPTION: Update auto exclude enable as " + flag + " using webpa");
		LOGGER.info("STEP " + stepCount
				+ ": ACTION: Execute webpa command:Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.FWUpdate.AutoExcluded.Enable as "
				+ flag);
		LOGGER.info("STEP " + stepCount + ": EXPECTED: Webpa set operation should success and auto excluded should be "
				+ flag);
		LOGGER.info("******************************************************************************");
		errorMessage = "Failed to update the parameter Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.FWUpdate.AutoExcluded.Enable as "
				+ flag + " using webpa";
		status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
				BroadBandWebPaConstants.WEBPA_PARAM_FWUPDATE_AUTO_EXCLUDED_ENABLE, BroadBandTestConstants.CONSTANT_3,
				flag, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		if (status) {
			LOGGER.info("STEP " + stepCount + ": ACTUAL: Successfully verified auto excluded enable as " + flag);
		} else {
			LOGGER.error("STEP " + stepCount + ": ACTUAL: " + errorMessage);
		}
		tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
		// ##################################################################################################//
		return stepCount;
	}

	public static int configureAutoExcludeUrlParameter(Dut device, AutomaticsTapApi tapEnv, String testCaseId,
			int stepCount, String xconfUrl, boolean isFileRemovalRequired) {
		String stepNumber = null;
		boolean status = false;
		String errorMessage = null;

		stepNumber = "s" + stepCount;
		status = false;
		LOGGER.info("******************************************************************************");
		LOGGER.info("STEP " + stepCount + ": DESCRIPTION: Update Xconf url as " + xconfUrl
				+ " using webpa or dmcli & delete swupdate.conf file");
		LOGGER.info("STEP " + stepCount
				+ ": ACTION: Execute webpa or dmcli command: Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.FWUpdate.AutoExcluded.XconfUrl string "
				+ xconfUrl + " & rm -rf /nvram/swupdate.conf");
		LOGGER.info("STEP " + stepCount + ": EXPECTED: Xconf url should be " + xconfUrl
				+ " and swupdate.conf file should be removed successfully");
		LOGGER.info("******************************************************************************");
		errorMessage = "Failed to update the parameter Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.FWUpdate.AutoExcluded.XconfUrl as "
				+ xconfUrl + " or failed to delete the swupdate.conf file";
		if (xconfUrl.equalsIgnoreCase(BroadBandTestConstants.EMPTY_STRING)) {
			status = DmcliUtils.verifyElseSetParameterUsingDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_FWUPDATE_AUTO_EXCLCUDED_XCONF_URL,
					BroadBandTestConstants.DMCLI_SUFFIX_TO_SET_STRING_PARAMETER, xconfUrl);
		} else {
			status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_FWUPDATE_AUTO_EXCLCUDED_XCONF_URL,
					BroadBandTestConstants.CONSTANT_0, xconfUrl, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		}

		if (isFileRemovalRequired && status) {
			status = false;
			status = CommonUtils.deleteFile(device, tapEnv, BroadBandTestConstants.SOFTWARE_UPDATE_CONF_FILE);
		}
		if (status) {
			LOGGER.info("STEP " + stepCount + ": ACTUAL: Successfully updated the auto exclude xconf url as " + xconfUrl
					+ " and removed swupdate.conf file");
		} else {
			LOGGER.error("STEP " + stepCount + ": ACTUAL: " + errorMessage);
		}
		tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
		// ##################################################################################################//
		return stepCount;
	}

	/**
	 * Verify whether multiple udhcpc.script error messages getting printed in
	 * Consolelog.txt
	 * <ol>
	 * <li>Upgrade the device to latest build by using TR-181</li>
	 * <li>Upgrade the device to latest build by using SNMP, IF CDL BY WEBPA
	 * FAILED</li>
	 * <li>Verify whether /etc/udhcpc.script error messages are getting printed in
	 * the Consolelog.txt.0 file</li>
	 * <li>PostCondition1.Revert back to the existing image</li>
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * 
	 * @author Deepika Sekar
	 * @refactor Govardhan
	 */
	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = BroadBandTestGroup.SYSTEM)
	@TestDetails(testUID = "TC-RDKB-SYSTEM-5050")
	public void testVerifyConsoleLogFirmwareUpgarde(Dut device) {

		// Variable Declaration begins
		String testCaseId = "TC-RDKB-SYSTEM-550";
		String stepNum = "s1";
		String response = null;
		String errorMessage = null;
		boolean hasLatestBuildChanged = false;
		boolean hasOriginalBuildChanged = false;
		String initialFirmwareVersion = null;
		boolean status = false;
		// Variable Declaration Ends

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-SYSTEM-5050");
		LOGGER.info(
				"TEST DESCRIPTION: Verify whether multiple udhcpc.script error messages getting printed in Consolelog.txt");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1.Upgrade the device to latest build by using TR-181");
		LOGGER.info("2.Upgrade the device to latest build by using SNMP, IF CDL BY WEBPA FAILED");
		LOGGER.info(
				"3. Verify whether /etc/udhcpc.script error messages are getting printed in the Consolelog.txt.0 file");
		LOGGER.info("PostCondition1. Revert back to the existing image");
		LOGGER.info("#######################################################################################");

		try {

			stepNum = "s1";
			errorMessage = "Unable to get latest image for current box model";
			status = false;
			boolean clearXconfConfig = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION : Upgrade the device to latest build by using TR-181");
			LOGGER.info("STEP 1: ACTION : TRIGGER THE CDL WITH LATEST FIRMWARE VERSION USING TR-181");
			LOGGER.info("STEP 1: EXPECTED : CDL MUST SUCCESSFUL WITH LATEST FIRMWARE VERSION USING TR-181.");
			LOGGER.info("**********************************************************************************");
			errorMessage = "UNABLE TO TRIGGER THE IMAGE UPGRADE ON THE DEVICE WITH LATEST FIRMWARE VERSION BY USING WEBPA.";
			initialFirmwareVersion = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device);
			LOGGER.info("CURRENT FIRMWARE VERSION: " + initialFirmwareVersion);
			// clearing xconf configurations in device
			clearXconfConfig = BroadBandXconfCdlUtils.toClearCdlInfoInXconf(device, tapEnv);
			LOGGER.info("XCONF CONFIGURATION CLEARED SUCCESSFULLY ON THE DEVICE: " + clearXconfConfig);
			try {
				hasLatestBuildChanged = FirmwareDownloadUtils.getLatestAvailableImageAndTriggerCdl(tapEnv, device,
						initialFirmwareVersion);
			} catch (TestException testException) {
				// Log & Suppress the Exception
				LOGGER.error(testException.getMessage());
				hasLatestBuildChanged = false;
			}
			status = hasLatestBuildChanged;
			LOGGER.info("FLASHED THE LATEST BUILD ON THE DEVICE BY USING WEBPA: " + status);
			if (status) {
				LOGGER.info("STEP 1: ACTUAL : CDL SUCCESSFUL WITH LATEST FIRMWARE VERSION BY WEBPA");
			} else {
				LOGGER.error("STEP 1: ACTUAL :" + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			if (!hasLatestBuildChanged) {
				stepNum = "s2";
				errorMessage = "Unable to get latest image for current box model";
				status = false;
				LOGGER.info("**********************************************************************************");
				LOGGER.info(
						"STEP 2: DESCRIPTION : Upgrade the device to latest build by using SNMP, IF CDL BY WEBPA FAILED ");
				LOGGER.info(
						"STEP 2: ACTION : TRIGGER THE CDL WITH LATEST FIRMWARE VERSION USING SNMP, IF CDL BY WEBPA FAILED");
				LOGGER.info(
						"STEP 2: EXPECTED : CDL MUST SUCCESSFUL WITH LATEST FIRMWARE VERSION BY USING SNMP, IF CDL BY WEBPA FAILED");
				LOGGER.info("**********************************************************************************");
				errorMessage = "UNABLE TO TRIGGER THE IMAGE UPGRADE ON THE DEVICE WITH LATEST FIRMWARE VERSION by SNMP.";
				initialFirmwareVersion = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device);
				LOGGER.info("CURRENT FIRMWARE VERSION: " + initialFirmwareVersion);
				try {
					hasLatestBuildChanged = FirmwareDownloadUtils
							.triggerAndWaitForTftpCodeDownloadUsingDocsisSnmpCommand(tapEnv, device,
									BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
											BroadBandPropertyKeyConstants.PARTIAL_PROPERTY_KEY_FOR_GA_IMAGE)
											+ BroadBandCdlConstants.BIN_EXTENSION,
									false);
				} catch (TestException testException) {
					// Log & Suppress the Exception
					LOGGER.error(testException.getMessage());
					hasLatestBuildChanged = false;
				}
				status = hasLatestBuildChanged;
				LOGGER.info("FLASHED THE LATEST BUILD ON THE DEVICE by SNMP: " + status);
				if (status) {
					LOGGER.info("STEP 2: ACTUAL : CDL SUCCESSFUL WITH LATEST FIRMWARE VERSION by SNMP");
				} else {
					LOGGER.error("STEP 2: ACTUAL :" + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
			} else {
				stepNum = "s2";
				errorMessage = "STEP 2: ACTUAL : UPGRADE BY WEBPA IS SUCCESSFUL SO SNMP UPGRADE IS NOT NEEDED";
				LOGGER.info("STEP 2: ACTUAL : UPGRADE BY WEBPA IS SUCCESSFUL SO SNMP UPGRADE IS NOT NEEDED");
				LOGGER.info("*******************************************************************************");
				tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			}

			stepNum = "s3";
			errorMessage = "Validate /etc/udhcpc.script: line error messages are getting printed in the Consolelog.txt.0 file";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 3: DESCRIPTION : Verify whether Validate /etc/udhcpc.script: line error messages are getting printed in the Consolelog.txt.0 file");
			LOGGER.info(
					"STEP 3: ACTION : Execute commands: \"'grep -i \\\"/etc/udhcpc.script\\\" /rdklogs/logs/Consolelog.txt.0'\"");
			LOGGER.info(
					"STEP 3: EXPECTED : Validate /etc/udhcpc.script: line error messages should not get displayed in the Consolelog.txt");
			LOGGER.info("**********************************************************************************");

			response = BroadBandCommonUtils.searchLogFilesInAtomOrArmConsoleByPolling(device, tapEnv,
					BroadBandTestConstants.UDHCPC_ERROR_MESSAGE,
					BroadBandCdlConstants.CONSOLE_LOG_FILE_TO_VERIFY_TELEMETRY_MARKER,
					BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			status = CommonMethods.isNull(response);
			if (status) {
				LOGGER.info(
						"STEP 3: ACTUAL : /etc/udhcpc.script error messages is not getting printed in ConsoleLog.txt");
			} else {
				LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		} finally {
			LOGGER.info("########################### STARTING POST-CONFIGURATIONS ###########################");
			try {
				int postConStepNumber = 0;
				BroadBandPostConditionUtils.executePostConditionToTriggerCdl(device, tapEnv, hasLatestBuildChanged,
						hasOriginalBuildChanged, postConStepNumber, initialFirmwareVersion);
			} catch (Exception exception) {
				LOGGER.error("Failed to get the BridgeMode status due to exception:" + exception.getMessage());
			}
			LOGGER.info("########################### END POST-CONFIGURATIONS ###########################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-SYSTEM-5050");
	}

	/**
	 *
	 * Test Case : Validate firmware Upgrade Time Check via xconf
	 *
	 * <p>
	 * STEPS:
	 * </p>
	 * <ol>
	 * <li>Step 1: Verify Triggered CDL Successful with latest firmware version
	 * through xconf</li>
	 * <li>Step 2: Verify log information "DEVICE_INIT" is available in
	 * Consolelog.txt.0/ArmConsolelog.txt.0 log after upgrade</li>
	 * <li>Step 3: Verify log information "Firmware upgrade start time" is available
	 * in /rdklogs/logs/xconf.txt.0 log after upgrade</li>
	 * <li>Step 4: Verify log information "Firmware upgrade end time" is available
	 * in /rdklogs/logs/xconf.txt.0 log after upgrade</li>
	 * <li>Step 5: Verify the upgrade time via xconf</li>
	 * <li>Step 6: Verify Triggered CDL Successful with previuos firmware version
	 * through xconf</li>
	 * <li>Step 7: Verify log information "DEVICE_INIT" is available in
	 * Consolelog.txt.0/ArmConsolelog.txt.0 log after downgrade</li>
	 * <li>Step 8: Verify log information "Firmware upgrade start time" is available
	 * in /rdklogs/logs/xconf.txt.0 log after downgrade</li>
	 * <li>Step 9: Verify log information "Firmware upgrade end time" is available
	 * in /rdklogs/logs/xconf.txt.0 log after downgrade</li>
	 * <li>Step 10: Verify the downgrade time via xconf</li>
	 * <li>POST CONDITION 1 : Verify that device is upgraded with the previous
	 * firmware version in #S6, if not try CDL again</li>
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * 
	 * @author Muthukumar
	 * @Refactor Sruthi Santhosh
	 *
	 */
	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
			BroadBandTestGroup.XCONF_CODE_DOWNLOAD })
	@TestDetails(testUID = "TC-RDKB-XCONF-UP-DWN-5001")
	public void testToVerifyXconfUpgradeAndDowngrade(Dut device) {
		// Variable Declaration begins
		String testCaseId = "TC-RDKB-XCONF-UP-DWN-501";
		int stepNumber = 1;
		String stepNum = "S" + stepNumber;
		String errorMessage = null;
		boolean status = false;
		boolean hasLatestBuildChanged = false;
		boolean hasOriginalBuildChanged = false;
		String initialFirmwareVersion = null;
		String latestFirmwareVersion = null;
		String cdlMessage = null;
		long startTimeXconf = 0;
		long endTimeXconf = 0;
		String isEthwanEnable = null;
		// Variable Declaration Ends
		try {
			LOGGER.info("#######################################################################################");
			LOGGER.info("STARTING TEST CASE: TC-RDKB-XCONF-UP-DWN-5001");
			LOGGER.info("TEST STEPS : ");
			LOGGER.info("STEP 1: DESCRITPION: Verify device is in ETHWAN/DOCSIS Mode.");
			LOGGER.info("STEP 2 : Verify Triggered CDL Successful with latest firmware version through xconf.");
			LOGGER.info(
					"STEP 3 : Verify log information 'DEVICE_INIT' is available in Consolelog.txt.0/ArmConsolelog.txt.0 log after upgrade.");
			LOGGER.info(
					"STEP 4 : Verify log information 'Firmware upgrade start time' is available in /rdklogs/logs/xconf.txt.0 log after upgrade.");
			LOGGER.info(
					"STEP 5 : Verify log information 'Firmware upgrade end time' is available in /rdklogs/logs/xconf.txt.0 log after upgrade.");
			LOGGER.info("STEP 6 : Verify the upgrade time via xconf.");
			LOGGER.info("STEP 7 : Verify  Triggered CDL Successful with previuos firmware version through xconf.");
			LOGGER.info(
					"STEP 8 : Verify log information 'DEVICE_INIT' is available in Consolelog.txt.0/ArmConsolelog.txt.0 log after downgrade.");
			LOGGER.info(
					"STEP 9 : Verify log information 'Firmware upgrade start time' is available in /rdklogs/logs/xconf.txt.0 log after downgrade.");
			LOGGER.info(
					"STEP 10 : Verify log information 'Firmware upgrade end time' is available in /rdklogs/logs/xconf.txt.0 log after downgrade.");
			LOGGER.info("STEP 11 : Verify the downgrade time via xconf.");
			LOGGER.info("STEP 12: DESCRITPION: Verify device DOCSIS/ETHWAN Mode persistence after Image Upgrade");
			LOGGER.info(
					"POST CONDITION 1 : Verify that device is upgraded with the previous firmware version in #S6, if not try CDL again");
			LOGGER.info("#######################################################################################");

			/**
			 * Step 1 : VERIFY TRIGGERED CDL SUCCESSFULL WITH LATEST FIRMWARE VERSION
			 * THROUGH XCONF
			 */

			stepNum = "S" + stepNumber;
			errorMessage = "Unable to verify device DOCIS mode (DOCSIS/ETHWAN).";
			status = false;
			LOGGER.info("*******************************************************************************************");
			LOGGER.info("STEP 1: DESCRITPION: Verify device is in ETHWAN/DOCSIS Mode.");
			LOGGER.info("STEP 1: ACTION:  Execute dmcli cmd Device.Ethernet.X_RDKCENTRAL-COM_WAN.Enabled ");
			LOGGER.info("STEP 1: EXPECTED: Value should return TRUE for ETHWAN.");
			LOGGER.info("*******************************************************************************************");
			try {

				isEthwanEnable = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_ETHWAN_MODE_ENABLE);
				if (CommonMethods.isNotNull(isEthwanEnable) && CommonMethods.isRunningEthwanMode()) {
					LOGGER.info("Value of ETHWAN enable parameter is not true while in ETHWAN Mode");
					status = isEthwanEnable.equalsIgnoreCase(BroadBandTestConstants.TRUE);
				} else {
					LOGGER.info("Value of ETHWAN enable parameter is not false as device is in DOCSIS Mode");
					status = isEthwanEnable.equalsIgnoreCase(BroadBandTestConstants.FALSE);
				}
			} catch (TestException exception) {
				errorMessage = errorMessage + " Exception occurred while getting Device ETHWAN Mode-> "
						+ exception.getMessage();
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info("STEP 1: ACTUAL : Successfully verified device Operational Mode.");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			/**
			 * Step 2 : VERIFY TRIGGERED CDL SUCCESSFULL WITH LATEST FIRMWARE VERSION
			 * THROUGH XCONF
			 */
			stepNumber = 2;
			stepNum = "S" + stepNumber;
			status = false;
			String cdlLogsForValidation = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Verify triggered cdl successfull with latest firmware version through xconf.");
			LOGGER.info("STEP " + stepNumber + ": ACTION : Trigger the cdl with latest firmware version through xconf");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : Cdl must successful with latest firmware version.");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to trigger the image upgrade on the device with latest firmware version.";
			initialFirmwareVersion = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device);
			LOGGER.info("CURRENT FIRMWARE VERSION: " + initialFirmwareVersion);
			latestFirmwareVersion = tapEnv.getLatestBuildImageVersionForCdlTrigger(device, false);
			LOGGER.info("LATEST FIRMWARE VERSION: " + latestFirmwareVersion);
			if (CommonMethods.isNull(latestFirmwareVersion)) {
				LOGGER.info(
						" GA image obtained from deployed version service is null. Hence getting the image from property file ");
				latestFirmwareVersion = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
						BroadBandPropertyKeyConstants.PARTIAL_PROPERTY_KEY_FOR_GA_IMAGE);
				LOGGER.info("Latest Firmware version from property file: " + latestFirmwareVersion);
			}
			LOGGER.info("LATEST FIRMWARE VERSION: " + latestFirmwareVersion);
			startTimeXconf = System.currentTimeMillis();
			cdlLogsForValidation = BroadBandXconfCdlUtils.triggerXconfCodeDownload(device, tapEnv,
					latestFirmwareVersion);
			errorMessage = "Failed to get cdl logs from /rdklogs/logs/xconf.txt.0";
			status = CommonMethods.isNotNull(cdlLogsForValidation) && BroadBandXconfCdlUtils.verifyCdlandReboot(device,
					tapEnv, latestFirmwareVersion, cdlLogsForValidation);
			endTimeXconf = System.currentTimeMillis();
			LOGGER.info("FLASHED THE LATEST BUILD ON THE DEVICE: " + status);
			if (status) {
				hasLatestBuildChanged = status;
				LOGGER.info(
						"STEP " + stepNumber + " : ACTUAL : Cdl successful with latest firmware version through xconf");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL :" + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			/**
			 * Step 3-6 : VERIFY XCONF LOG INFORMATION AFTER UPGRADE
			 */
			cdlMessage = "upgrade";
			stepNumber++;
			executeTestStepsToValidateXconfLogs(device, tapEnv, testCaseId, stepNumber, latestFirmwareVersion,
					cdlLogsForValidation, startTimeXconf, endTimeXconf, cdlMessage);

			/**
			 * Step 7 : VERIFY TRIGGERED CDL SUCCESSFULL WITH PREVIOUS FIRMWARE VERSION
			 * THROUGH XCONF
			 */
			stepNumber = 7;
			stepNum = "S" + stepNumber;
			status = false;
			cdlLogsForValidation = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Verify triggered cdl successfull with previous firmware version through xconf ");
			LOGGER.info(
					"STEP " + stepNumber + ": ACTION : Trigger the cdl with previous firmware version through xconf");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : Cdl must successful with previous firmware version.");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to trigger the image upgrade on the device with previous firmware version.";
			startTimeXconf = System.currentTimeMillis();
			cdlLogsForValidation = BroadBandXconfCdlUtils.triggerXconfCodeDownload(device, tapEnv,
					initialFirmwareVersion);
			errorMessage = "Failed to get cdl logs from /rdklogs/logs/xconf.txt.0";
			status = CommonMethods.isNotNull(cdlLogsForValidation) && BroadBandXconfCdlUtils.verifyCdlandReboot(device,
					tapEnv, initialFirmwareVersion, cdlLogsForValidation);
			endTimeXconf = System.currentTimeMillis();
			LOGGER.info("FLASHED THE ORIGINAL BUILD ON THE DEVICE: " + status);
			if (status) {
				hasOriginalBuildChanged = status;
				LOGGER.info("STEP " + stepNumber + " : ACTUAL : Cdl successful with previous firmware version.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL :" + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			/**
			 * Step 8-11 : VERIFY XCONF LOG INFORMATION AFTER DOWNGRADE
			 */
			cdlMessage = "downgrade";
			stepNumber++;
			executeTestStepsToValidateXconfLogs(device, tapEnv, testCaseId, stepNumber, initialFirmwareVersion,
					cdlLogsForValidation, startTimeXconf, endTimeXconf, cdlMessage);

			/**
			 * Step 12: Verify ETHWAN Mode persists after Image Upgrade
			 */
			stepNumber = 12;
			stepNum = "S" + stepNumber;
			errorMessage = "Device DOCSIS/ETHWAN Mode did not persist after Image Upgrade";
			status = false;
			LOGGER.info("*******************************************************************************************");
			LOGGER.info("STEP 12: DESCRITPION: Verify device DOCSIS/ETHWAN Mode persistence after Image Upgrade");
			LOGGER.info("STEP 12: ACTION:  Execute dmcli cmd Device.Ethernet.X_RDKCENTRAL-COM_WAN.Enabled ");
			LOGGER.info("STEP 12: EXPECTED: Value should return TRUE/False from previous step.");
			LOGGER.info("*******************************************************************************************");
			try {
				status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_ETHWAN_MODE_ENABLE, isEthwanEnable,
						BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);
			} catch (TestException exception) {
				errorMessage = errorMessage + " Exception occurred while getting device operational mode "
						+ exception.getMessage();
				LOGGER.error(errorMessage);

			}

			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ ": ACTUAL : Successfully verified device operational mode persistence after image downgrade");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("EXCEPTION OCCURRED WHILE VALIDATING FIRMWARE UPGRADE TIME CHECK VIA XCONF : " + errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					true);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			/**
			 * POST CONDITION : VERIFY THAT DEVICE IS UPGRADED WITH THE PREVIOUS FIRMWARE
			 * VERSION, IF NOT TRY CDL AGAIN
			 */

			if (!hasOriginalBuildChanged) {
				BroadBandPostConditionUtils.executePostConditionToTriggerCdl(device, tapEnv, hasLatestBuildChanged,
						hasOriginalBuildChanged, BroadBandTestConstants.CONSTANT_1, initialFirmwareVersion);
			}
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-XCONF-UP-DWN-5001");
		LOGGER.info("#######################################################################################");
	}

	/**
	 * Test method used to validate the XCONF log information and calculate the
	 * upgrade/downgrade time
	 * 
	 * @param device                instance of{@link Dut}
	 * @param tapEnv                instance of {@link AutomaticsTapApi}
	 * @param testCaseId            Test case ID
	 * @param stepNumber            Step Number
	 * @param latestFirmwareVersion FirmwareVersion To CDL
	 * @param cdlLogsForValidation  CDL logs for validation
	 * @param startTimeXconf        start time for xconf cdl
	 * @param endTimeXconf          end time for xconf cdl
	 * @param cdlMessage            upgrade/downgrade
	 * @Refactor Sruthi Santhosh
	 */
	public static void executeTestStepsToValidateXconfLogs(Dut device, AutomaticsTapApi tapEnv, String testCaseId,
			int stepNumber, String latestFirmwareVersion, String cdlLogsForValidation, long startTimeXconf,
			long endTimeXconf, String cdlMessage) {
		String stepNum = "S" + stepNumber;
		String errorMessage = null;
		boolean status = false;
		String searchLogMessage = null;
		String pattenMatcher = null;

		/**
		 * SETP : VERIFY LOG INFORMATION "DEVICE_INIT" IS AVAILABLE IN
		 * CONSOLELOG.TXT.0/ARMCONSOLELOG.TXT.0
		 */
		stepNum = "S" + stepNumber;
		status = false;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ " : DESCRIPTION : Verify log information 'DEVICE_INIT' is available in Consolelog.txt.0/ArmConsolelog.txt.0 log after "
				+ cdlMessage);
		LOGGER.info("STEP " + stepNumber
				+ " : ACTION :  grep -i 'DEVICE_INIT' /rdklogs/logs/Consolelog.txt.0 or /rdklogs/logs/ArmConsolelog.txt.0");
		LOGGER.info("STEP " + stepNumber
				+ " : EXPECTED : Expected Log 'DEVICE_INIT' should be available In Consolelog.txt.0/ArmConsolelog.txt.0 log");
		LOGGER.info("**********************************************************************************");
		errorMessage = "Unable to verify log message 'DEVICE_INIT' in Consolelog.txt.0/ArmConsolelog.txt.0";
		String fileName = DeviceModeHandler.isDSLDevice(device) ? BroadBandTestConstants.RDKLOGS_LOGS_ARM_CONSOLE_0
				: BroadBandTestConstants.RDKLOGS_LOGS_CONSOLE_TXT_0;
		searchLogMessage = BroadBandCommonUtils.concatStringUsingStringBuffer(
				BroadBandTraceConstants.TELEMETRY_MARKER_DEVICE_INITIATION, AutomaticsConstants.COLON);
		pattenMatcher = BroadBandCommonUtils.concatStringUsingStringBuffer(searchLogMessage,
				BroadBandCommonUtils.removeDifferentSignedExtensionsInRequestedBuildName(latestFirmwareVersion));
		String response = BroadBandCommonUtils.searchLogFiles(tapEnv, device, searchLogMessage, fileName,
				BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		status = CommonMethods.patternMatcher(response, pattenMatcher)
				&& CommonMethods.patternMatcher(response, BroadBandTestConstants.STRING_REGEX_DATE);
		if (status) {
			LOGGER.info("STEP " + stepNumber + " : ACTUAL : Successfully verified the log message '" + searchLogMessage
					+ "' in " + fileName);
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

		/**
		 * SETP : VERIFY LOG INFORMATION "FIRMWARE UPGRADE START TIME" IS AVAILABLE IN
		 * /RDKLOGS/LOGS/XCONF.TXT.0
		 */
		stepNumber++;
		stepNum = "S" + stepNumber;
		status = false;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ " : DESCRIPTION : Verify log information 'Firmware upgrade start time'  is available in /rdklogs/logs/xconf.txt.0 log after "
				+ cdlMessage);
		LOGGER.info(
				"STEP " + stepNumber + " : ACTION : grep -i 'Firmware upgrade start time' /rdklogs/logs/xconf.txt.0");
		LOGGER.info("STEP " + stepNumber
				+ " : EXPECTED : Expected Log 'Firmware upgrade start time' should be available in /rdklogs/logs/xconf.txt.0 ");
		LOGGER.info("**********************************************************************************");
		errorMessage = "Unable to verify the 'Firmware upgrade start time' in /rdklogs/logs/xconf.txt.0 log";
		pattenMatcher = BroadBandCommonUtils.concatStringUsingStringBuffer(
				BroadBandTraceConstants.LOG_MESSAGE_FIRMWARE_START_TIME, AutomaticsConstants.SPACE,
				AutomaticsConstants.COLON, AutomaticsConstants.SPACE, BroadBandTestConstants.PATTERN_FOR_UN_SIGNED_INT);
		status = CommonUtils.patternSearchFromTargetString(cdlLogsForValidation, pattenMatcher);
		if (status) {
			LOGGER.info("STEP " + stepNumber
					+ " : ACTUAL : Successfully verified the log message 'Firmware upgrade start time' in /rdklogs/logs/xconf.txt.0");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

		/**
		 * SETP : VERIFY LOG INFORMATION "FIRMWARE UPGRADE END TIME" IS AVAILABLE IN
		 * XCONF.TXT.0
		 */
		stepNumber++;
		stepNum = "S" + stepNumber;
		status = false;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ " : DESCRIPTION : Verify log information 'Firmware upgrade end time'  is available in /rdklogs/logs/xconf.txt.0 log after "
				+ cdlMessage);
		LOGGER.info("STEP " + stepNumber + " : ACTION : grep -i 'Firmware upgrade end time' /rdklogs/logs/xconf.txt.0");
		LOGGER.info("STEP " + stepNumber
				+ " : EXPECTED : Expected Log 'Firmware upgrade end time' should be available in /rdklogs/logs/xconf.txt.0 ");
		LOGGER.info("**********************************************************************************");
		errorMessage = "Unable to verify the 'Firmware upgrade end time' in /rdklogs/logs/xconf.txt.0 log";
		pattenMatcher = BroadBandCommonUtils.concatStringUsingStringBuffer(
				BroadBandTraceConstants.LOG_MESSAGE_FIRMWARE_END_TIME, AutomaticsConstants.SPACE,
				AutomaticsConstants.COLON, AutomaticsConstants.SPACE, BroadBandTestConstants.PATTERN_FOR_UN_SIGNED_INT);
		status = CommonUtils.patternSearchFromTargetString(cdlLogsForValidation, pattenMatcher);
		if (status) {
			LOGGER.info("STEP " + stepNumber
					+ " : ACTUAL : Successfully verified the log message 'Firmware upgrade end time' in /rdklogs/logs/xconf.txt.0");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

		/**
		 * STEP : VERIFY THE UPGRADE/DOWNGRADE TIME VIA XCONF
		 */
		stepNumber++;
		stepNum = "S" + stepNumber;
		status = false;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify the " + cdlMessage + " time via xconf.");
		LOGGER.info("STEP " + stepNumber + ": ACTION : Execute webpa command : calculate the " + cdlMessage + " time");
		LOGGER.info("STEP " + stepNumber + ": EXPECTED : " + cdlMessage + " time should be in 8 -10 mins");
		LOGGER.info("**********************************************************************************");
		errorMessage = "Unable to verify the " + cdlMessage + " time via xconf";
		status = startTimeXconf - endTimeXconf < BroadBandTestConstants.TEN_MINUTE_IN_MILLIS;
		if (status) {
			LOGGER.info("STEP " + stepNumber + ": ACTUAL: Successfully verified the " + cdlMessage + " time via xconf");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL :" + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	}
	
	
    /**
     * 
     * Test Case # 1: Verify Periodic firmware upgrade check
     * 
     * <p>
     * STEPS:
     * </p>
     * <ol>
     * <li>STEP 1: Verify default value for periodic firmware upgrade using WebPA</li>
     * <li>STEP 2: Enable periodic firmware upgrade through webPA command</li>
     * <li>STEP 3: Configure XCONF server with required configuration using REST API</li>
     * <li>STEP 4: Verify currently running cron job details using crontab -l command</li>
     * <li>STEP 5: Verify cdl trigger cron job fro Atom based device is currently running</li>
     * <li>STEP 6: Configure cron job for Atom based device with 2 minutes time interval</li>
     * <li>STEP 7: Verify Code download triggered status</li>
     * <li>STEP 8: Wait for 3 minutes and again verify periodic code download triggered status</li>
     * <li>STEP 9: Verify code download logs from xconf.txt.0 file</li>
     * <li>STEP 10: Configure cron job for atom based device with default time interval</li>
     * <li>POST CONDITION:</li>
     * <li>1. Disable periodic firmware upgrade parameter</li>
     * </ol>
     * 
     * @author Gnanaprakasham S
	 * @refactor Athira
     * 
     * @param device
     *            {@link Dut}
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
    	    BroadBandTestGroup.XCONF_CODE_DOWNLOAD })
    @TestDetails(testUID = "TC-RDKB-XCONF-PERIFWUP-1005")
    public void testPeriodicFirmwareUpgradeCheck(Dut device) {
    	// Variable to store error message
    	String errorMessage = null;
    	// variable to store testcaseId
    	String testCaseId = "TC-RDKB-XCONF-PERIFWUP-005";
    	// variable to store test step number
    	String testStepNumber = "s1";
    	// variable to store the status of each step
    	boolean status = false;
    	// variable to store the current image in the box, to revert back to the same image in post condition
    	String currentImageName = null;
    	// variable to store response
    	String response = null;
    	// Variable to store periodic firmware enabled status
    	boolean isPeriodicFirmwareEnabled = false;
    	// Variable to store Atom based device cdl trigger cron job default time interval
    	String cronDefaultValue = null;
    	String model = device.getModel();
    	// variable to hold post condition initial number
    	int postConStepNumber = 0;
    	// variable to store status of cdl data posted
    	boolean isCdlDataPosted = false;
    	
    	try {
    		
    	    LOGGER.info("************************************************************************************");
    	    LOGGER.info("STARTING TEST CASE: TC-RDKB-XCONF-PERIFWUP-1005");
    	    LOGGER.info("TEST DESCRIPTION: Schedule Periodic Firmware Upgrade Check");

    	    LOGGER.info("TEST STEPS : ");
    	    LOGGER.info("1. Verify default value for periodic firmware upgrade using WebPA");
    	    LOGGER.info("2. Enable periodic firmware upgrade through webPA command");
    	    LOGGER.info("3. Configure XCONF server with required configuration using REST API");
    	    LOGGER.info("4. Verify currently running cron job deatils using crontab -l command");
    	    LOGGER.info("5. Verify cdl trigger cron job for atom based device is currently running ");
    	    LOGGER.info("6. Configure cron job for atom based device with 2 minutes time interval");
    	    LOGGER.info("7. Verify Code download triggered status");
    	    LOGGER.info("8. Wait for 3 minutes and again verify periodic code download triggerred status");
    	    LOGGER.info("9. Verify code download logs from xconf.txt.0 file");
    	    LOGGER.info("10. Configure cron job for atom based device with default time interval");
    	    LOGGER.info("************************************************************************************");

    	    LOGGER.info("**********************************************************************************");
    	    LOGGER.info("STEP 1: DESCRIPTION : Verify default value for periodic firmware upgrade using WebPA");
    	    LOGGER.info(
    		    "STEP 1: ACTION : Execute Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.PeriodicFWCheck.Enable parameter using WebPA/Dmcli");
    	    LOGGER.info("STEP 1: EXPECTED : WebPA should return success message with value false");
    	    LOGGER.info("**********************************************************************************");
    	    
    	    errorMessage = "Periodic firmware check is not disabled by default";
    	    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
    		    BroadBandWebPaConstants.WEBPA_PARAM_PERIODIC_FW_CHECK_ENABLE);

    	    status = CommonMethods.isNotNull(response) && !Boolean.parseBoolean(response);

    	    if (!status) {
    		LOGGER.info(
    			"It seems periodic firmware download is enabled by default. It may be because of RFC rule. So check the RFC configuration");
    		response = tapEnv.executeCommandUsingSsh(device,
    			BroadBandTestConstants.CMD_GET_PERIODIC_FIRMWARE_CHECK_VALUE_RFC_CONFIG_FILE);
    		if (CommonMethods.isNotNull(response)) {
    		    response = CommonMethods.patternFinder(response,
    			    BroadBandTestConstants.PATTERN_GET_PERIODIC_FIRMWARE_CHECK_VALUE_RFC_CONFIG_FILE);
    		    isPeriodicFirmwareEnabled = CommonMethods.isNotNull(response) && Boolean.parseBoolean(response);
    		}
    		if (isPeriodicFirmwareEnabled) {
    		    status = true;
    		    LOGGER.info(
    			    "Periodic firmware download is enabled by RFC rule configuration. Hence enabled by default is expected result");
    		} else {
    		    LOGGER.info("Periodic firmware download is not configured in RFC rule configuration as well");
    		}
    	    }
    	    
    	    if (status) {
    			LOGGER.info(
    				"STEP 1: ACTUAL : SUCCESSFULLY VERIFIED DEFAULT VALUE FOR PERIODIC FIRMWARE CHECK PARAMETER");
    		    } else {
    			LOGGER.error("STEP 1: ACTUAL :" + errorMessage);
    		    }
    		    LOGGER.info("**********************************************************************************");
    		    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

    		    testStepNumber = "s2";
    		    status = false;
    		    errorMessage = "Failed to Set value for WebPa parameter:"
    			    + BroadBandWebPaConstants.WEBPA_PARAM_PERIODIC_FW_CHECK_ENABLE + "with Value:"
    			    + BroadBandTestConstants.TRUE;

    		    LOGGER.info("**********************************************************************************");
    		    LOGGER.info("STEP 2: DESCRIPTION : Enable periodic firmware upgrade through webPA command");
    		    LOGGER.info(
    			    "STEP 2: ACTION : Set Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.PeriodicFWCheck.Enable parameter value as true using WebPA/Dmcli");
    		    LOGGER.info("STEP 2: EXPECTED : WebPA should return success message with value true");
    		    LOGGER.info("**********************************************************************************");

    		    if (!isPeriodicFirmwareEnabled) {
    			status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
    				BroadBandWebPaConstants.WEBPA_PARAM_PERIODIC_FW_CHECK_ENABLE, BroadBandTestConstants.CONSTANT_3,
    				BroadBandTestConstants.TRUE);
    		    } else {
    			status = true;
    		    }

    		    if (status) {
    			LOGGER.info("STEP 2: ACTUAL : SUCCESSFULLY VERIFIED PERIODIC FIRMWARE CHECK IS ENABLED");
    		    } else {
    			LOGGER.error("STEP 2: ACTUAL :" + errorMessage);
    		    }
    		    LOGGER.info("**********************************************************************************");
    		    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, true);
    		    
    		    testStepNumber = "s3";
    		    status = false;
    		    errorMessage = "Failed to configure Xconf server with MAC address and Firmware details for cdl";

    		    LOGGER.info("**********************************************************************************");
    		    LOGGER.info("STEP 3: DESCRIPTION : Configure XCONF server with required configuration using REST API");
    		    LOGGER.info("STEP 3: ACTION : Configure XCONF server with MAC address and firmware details");
    		    LOGGER.info(
    			    "STEP 3: EXPECTED : configure the code download configuration using XCONF Rest API and swupdate.conf should updated with xconf url");
    		    LOGGER.info("**********************************************************************************");

    		    currentImageName = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device);

    		    if (CommonMethods.isNotNull(currentImageName)) {
    			BroadBandXconfCdlUtils.configureRdkbDeviceForXconfCdl(tapEnv, device, currentImageName, false,
    				AutomaticsConstants.FIRMWARE_DOWNLOAD_PROTOCOL_HTTP);
    			status = true;
    			isCdlDataPosted = status;
    		    }

    		    if (status) {
    			LOGGER.info(
    				"STEP 3: ACTUAL : SUCCESSFULLY CONFIGURE XCONF SERVER WITH DEVICE MAC ADDRESS AND FIRMWARE DETAILS FOR CDL TRIGGER");
    		    } else {
    			LOGGER.error("STEP 3: ACTUAL :" + errorMessage);
    		    }
    		    LOGGER.info("**********************************************************************************");
    		    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, true);


    		    testStepNumber = "s4";
    		    status = false;
    		    errorMessage = "Failed to verify the cron job details using crontab -l command";

    		    LOGGER.info("**********************************************************************************");
    		    LOGGER.info("STEP 4: DESCRIPTION : Verify currently running cron job details using crontab command");
    		    LOGGER.info("STEP 4: ACTION : Execute \"crontab -l\" command and verify result");
    		    LOGGER.info("STEP 4: EXPECTED : crontab -l command must provide the current running cronjob details");
    		    LOGGER.info("**********************************************************************************");

    		    response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CRONTAB_EXECUTE_COMMAND);

    		    if (CommonMethods.isNotNull(response)
    			    && !response.contains(BroadBandTestConstants.NO_SUCH_FILE_OR_DIRECTORY)) {
    			status = true;
    		    } else {
    			errorMessage = "Failed to get the cron job details using crontab -l command";
    			LOGGER.error(errorMessage);
    		    }

    		    if (status) {
    			LOGGER.info(
    				"STEP 4: ACTUAL : SUCCESSFULLY VERIFIED CURRENTLY RUNNING CRON JOB DETAILS USING crontab -l COMMAND");
    		    } else {
    			LOGGER.error("STEP 4: ACTUAL :" + errorMessage);
    		    }
    		    LOGGER.info("**********************************************************************************");
    		    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, true);

    		    testStepNumber = "s5";
    		    status = false;
    		    String firmwareDownloadScript = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(
    					device, BroadBandTestConstants.PROP_KEY_FIRMWARE_DOWNLOAD_SCRIPT);		
    		    errorMessage = "Failed to verify /etc/" + firmwareDownloadScript
    			    + " 2 cron job running status";  		 

    		    LOGGER.info("**********************************************************************************");
    		    LOGGER.info(
    			    "STEP 5: DESCRIPTION : Verify \"/etc/" + firmwareDownloadScript
    				    + " 2\" cdl trigger cron job is running or not");
    		    LOGGER.info("STEP 5: ACTION : Execute \"crontab -l\" command and verify result");
    		    LOGGER.info("STEP 5: EXPECTED : crontab -l command response must contain \"/etc/"
    			    + firmwareDownloadScript
    			    + " 2\" details (firmwareDwnld.sh script)");
    		    LOGGER.info("**********************************************************************************");      		 

    		    if (CommonMethods.isNotNull(response)
    			    && !response.contains(BroadBandTestConstants.NO_SUCH_FILE_OR_DIRECTORY)) {
    			status = response.contains("/etc/" + firmwareDownloadScript + " 2");    		 

    			if (status) {
    			    LOGGER.info(
    				    "SUCCESSFULLY VERIFIED /etc/" + firmwareDownloadScript
    					    + " CDL TRIGGER CRON JOB IS RUNNING ");   	   		 
    			}

    			LOGGER.info("Obtained response is : " + response);

    			cronDefaultValue = CommonMethods.patternFinder(response,
    				"(.*)  \\/etc\\/" + firmwareDownloadScript + " 2");
    			
    			LOGGER.info("Device model " + model + " cdl trigger cron job default time interval is  : "
        				+ cronDefaultValue);

    		    } else {
    			errorMessage = "It seems /etc/" + firmwareDownloadScript
    				+ " 2 cron job is not currently running";
    			LOGGER.error(errorMessage);
    		    }

    		    if (status) {
    			LOGGER.info("STEP 5: ACTUAL : SUCCESSFULLY VERIFIED /etc/"
    				+ firmwareDownloadScript + " CRON JOB IS RUNNING");
       		    } else {
    			LOGGER.error("STEP 5: ACTUAL :" + errorMessage);
    		    }
    		    LOGGER.info("**********************************************************************************");
    		    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, true);

    		    /**
    		     * Before triggering periodic trigger just clear the Xconf.txt log file
    		     */
    		    LOGGER.info("Before triggering periodic trigger just clear the Xconf.txt log file");
    		    tapEnv.executeCommandUsingSsh(device, "echo \"\" > /rdklogs/logs/xconf.txt.0");
    	}catch (Exception exception) {
    	    errorMessage = exception.getMessage();
    	    LOGGER.error("Exception Occurred while verifying periodic firmware upgrade check:" + errorMessage);
    	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, testStepNumber, status,
    		    errorMessage, true);
    	} finally {
    	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
    	    LOGGER.info("POST-CONDITION STEPS");

    	    /**
    	     * POST CONDITION 1 : CLEAR THE CDL INFORMATION IN XCONF SERVER
    	     */
    	    if (isCdlDataPosted) {
    		postConStepNumber++;
    		BroadBandPostConditionUtils.executePostConditionToClearCdlInfoInXconf(device, tapEnv,
    			postConStepNumber);
    	    }

    	    /**
    	     * POST CONDITION 2 : Disable periodic firmware upgrade value through webpa command
    	     */
    	    status = false;
    	    postConStepNumber++;
    	    errorMessage = "Failed to Set value for WebPa parameter:"
    		    + BroadBandWebPaConstants.WEBPA_PARAM_PERIODIC_FW_CHECK_ENABLE + "with Value:"
    		    + BroadBandTestConstants.FALSE;
    	    LOGGER.info("#######################################################################################");
    	    LOGGER.info("POST-CONDITION " + postConStepNumber
    		    + " : DESCRIPTION: Disable periodic firmware upgrade through webPA command.");
    	    LOGGER.info("POST-CONDITION " + postConStepNumber
    		    + " : ACTION : Execute Webpa Set command for following param: Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.PeriodicFWCheck.Enable & set value as false.");
    	    LOGGER.info("POST-CONDITION " + postConStepNumber
    		    + " : EXPECTED : periodic firmware upgrade should be disabled via Webpa.");
    	    LOGGER.info("#######################################################################################");
    	    if (!isPeriodicFirmwareEnabled) {
    		LOGGER.info("Disable periodic firmware upgrade through webPA command");
    		status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
    			BroadBandWebPaConstants.WEBPA_PARAM_PERIODIC_FW_CHECK_ENABLE, BroadBandTestConstants.CONSTANT_3,
    			BroadBandTestConstants.FALSE);
    	    }
    	    if (status) {
    		LOGGER.info(
    			"POST-CONDITION " + postConStepNumber + " ACTUAL : Successfully disabled periodic firmware upgrade.");
    	    } else {
    		LOGGER.error("POST-CONDITION " + postConStepNumber + " : ACTUAL : " + errorMessage);
    	    }
    	}
    }

}
