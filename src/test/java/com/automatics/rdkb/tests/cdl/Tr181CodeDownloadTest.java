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

import java.util.ArrayList;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandCdlConstants;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandPropertyKeyConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandPostConditionUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.cdl.BroadBandCodeDownloadUtils;
import com.automatics.rdkb.utils.cdl.CodeDownloadUtils;
import com.automatics.rdkb.utils.cdl.FirmwareDownloadUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

/**
 * Class for Validating the TR-181 Code Download using HTTP Protocol for VBN/
 * DEV Builds.
 */

public class Tr181CodeDownloadTest extends AutomaticsTestBase {

	/**
	 *
	 * Test Case : Verify the TR-181 Code Download using HTTP Protocol for VBN/DEV
	 * Builds is successful.
	 *
	 * <p>
	 * STEPS:
	 * </p>
	 * <ol>
	 * <li>S1) Verify setting the appropriate value for parameter:
	 * Device.DeviceInfo.X_RDKCENTRAL-COM_FirmwareDownloadURL</li>
	 * <li>S2) Verify setting the appropriate value for parameter:
	 * Device.DeviceInfo.X_RDKCENTRAL-COM_FirmwareToDownload</li>
	 * <li>S3) Verify triggering the code download using parameter:
	 * Device.DeviceInfo.X_RDKCENTRAL-COM_FirmwareDownloadNow</li>
	 * <li>S4) Verify the code download status is 'In Progress'.</li>
	 * <li>S5) Verify the code download status is 'Completed' after few
	 * minutes.</li>
	 * <li>S6) Verify the CDL is triggered successfully.</li>
	 * <li>S7) Verify the device reboots after successful code download.</li>
	 * <li>S8) Verify the device is accessible after successful code download &
	 * reboot.</li>
	 * <li>S9) Verify the image is flashed successfully.</li>
	 * </ol>
	 *
	 * @author BALAJI V
	 * @refactor Athira
	 * 
	 * @param device {@link Dut}
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
			TestGroup.CODE_DOWNLOAD })

	@TestDetails(testUID = "TC-RDKB-CDL-5502")
	public void testTr181CodeDownloadWithLatestBuild(Dut device) {
		String testCaseId = "TC-RDKB-CDL-552";
		boolean result = false;
		String errorMessage = null;
		String step = null;
		String searchResponse = null;
		String firmwareFileName = null;
		String currentFirmwareVersion = null;
		long pollDuration;
		long startTime;
		String logFile = null;
		String latestCdlImageBin = null;

		try {
			LOGGER.info("#######################################################################################");
			LOGGER.info("STARTING TEST CASE: TC-RDKB-CDL-5502");
			LOGGER.info(
					"TEST DESCRIPTION: Verify the TR-181 Code Download using HTTP Protocol for VBN/ DEV Builds is successful.");

			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("#######################################################################################");
			LOGGER.info("PRE-CONDITION 1 : DESCRIPTION : VERIFY NO PROD BUILD IS ON THE DEVICE.");
			LOGGER.info(
					"PRE-CONDITION 1 : ACTION : VERIFY BUILD USING WEBPA COMMAND - Device.DeviceInfo.X_CISCO_COM_FirmwareName");
			LOGGER.info("PRE-CONDITION 1 : EXPECTED : NO PROD BUILD SHOULD BE ON DEVICE.");
			LOGGER.info("#######################################################################################");
			errorMessage = "OBTAINED A NULL RESPONSE/DEVICE HAS PROD BUILD WHILE TRYING TO GET THE CURRENT FIRMWARE VERSION.";
			currentFirmwareVersion = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device);
			result = !CommonMethods.isNull(currentFirmwareVersion) && !BroadBandCommonUtils.isProdBuildOnDevice(device);
			if (result) {
				LOGGER.info("PRE-CONDITION 1 : ACTUAL : CURRENT FIRMWARE VERSION: " + currentFirmwareVersion
						+ " AND NO PROD BUILD IS ON DEVICE. HENCE PROCEEDING WITH TEST EXECUTION.");
			} else {
				LOGGER.error("PRE-CONDITION 1 : ACTUAL : " + errorMessage);
				throw new TestException(
						BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION 1: FAILED : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			LOGGER.info("PRE-CONDITION 2 : DESCRIPTION : VERIFY REQUESTED IMAGE VERSION IS AVAILABLE IN CDL SERVERS.");
			LOGGER.info("PRE-CONDITION 2 : ACTION : VERIFY REQUESTED IMAGE VERSION IS AVAILABLE IN CDL SERVERS");
			LOGGER.info("PRE-CONDITION 2 : EXPECTED : REQUESTED IMAGE VERSION SHOULD BE AVAILABLE IN CDL SERVERS.");
			LOGGER.info("#######################################################################################");
			errorMessage = "OBTAINED A NULL RESPONSE/REQUEST IMAGE VERSION IS NOT AVAILABLE IN CDL SERVERS WHILE TRYING TO GET THE LATEST FIRMWARE VERSION FOR CDL.";

			String latestCdlImage = tapEnv.getLatestBuildImageVersionForCdlTrigger(device, false);
			LOGGER.info("LATEST FIRMWARE VERSION: " + latestCdlImage);
			if (CommonMethods.isNull(latestCdlImage)) {
				LOGGER.info(
						" GA image obtained from deployed version service is null. Hence getting the image from property file ");
				latestCdlImage = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
						BroadBandPropertyKeyConstants.PARTIAL_PROPERTY_KEY_FOR_GA_IMAGE);
				LOGGER.info("Latest Firmware version from property file: " + latestCdlImage);
			}

			if (CommonMethods.isNull(latestCdlImage) && DeviceModeHandler.isDSLDevice(device)) {
				LOGGER.info("Didn't get image from RDK portal. So getting the image name from properties file");
				latestCdlImage = AutomaticsTapApi.getSTBPropsValue(BroadBandTestConstants.DSL_IMAGE);
			}
			if (!CommonMethods.isNull(latestCdlImage)) {
				latestCdlImageBin = latestCdlImage + BroadBandCdlConstants.BIN_EXTENSION;
			}
			result = !CommonMethods.isNull(latestCdlImage);
			if (result) {
				LOGGER.info("PRE-CONDITION 2 : ACTUAL : LATEST FIRMWARE VERSION WITH SIGNED & BIN EXTENSION: "
						+ latestCdlImageBin);
			} else {
				LOGGER.error("PRE-CONDITION 2 : ACTUAL : " + errorMessage);
				throw new TestException(
						BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION 1: FAILED : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			LOGGER.info("PRE-CONDITION 3 : DESCRIPTION : VERIFY CODE DOWNLOAD URL IS AVAILABLE.");
			LOGGER.info("PRE-CONDITION 3 : ACTION : URL TO BE VALIDATED");
			LOGGER.info("PRE-CONDITION 3 : EXPECTED : CODE DOWNLOAD URL SHOULD BE AVAILABLE.");
			LOGGER.info("#######################################################################################");
			// String to store the code Download server Url in stb properties

			String codeDownloadUrl = AutomaticsTapApi.getSTBPropsValue(BroadBandCdlConstants.PROP_KEY_CDL_URL);
			LOGGER.info("CODE DOWNLOAD URL: " + codeDownloadUrl);
			result = CommonMethods.isNotNull(codeDownloadUrl);
			if (result) {
				LOGGER.info("PRE-CONDITION 3 : ACTUAL : CODE DOWNLOAD URL: " + codeDownloadUrl);
			} else {
				errorMessage = "BLOCKING THE EXECUTION AS THE CODE DOWNLOAD URL : '" + codeDownloadUrl
						+ "', GOT FROM stb.properties WITH KEY : '" + BroadBandCdlConstants.PROP_KEY_CDL_URL
						+ "' IS NOT AVAILABLE. CHECK stb.properties FOR EXPECTED ENTRY, UPDATE AND RE-TRIGGER.";
				LOGGER.error("PRE-CONDITION 3 : ACTUAL : " + errorMessage);
				throw new TestException(
						BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION 1: FAILED : " + errorMessage);
			}
			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");
			int stepNumber = 1;
			/**
			 * S1) Verify setting the appropriate value for parameter:
			 * Device.DeviceInfo.X_RDKCENTRAL-COM_FirmwareDownloadURL
			 */
			step = "S" + stepNumber;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP :  " + stepNumber + " : DESCRIPTION : VERIFY SETTING THE FIRMWARE DOWNLOAD URL.");
			LOGGER.info("STEP :  " + stepNumber
					+ " : ACTION : EXECUTE SET COMMAND ON Device.DeviceInfo.X_RDKCENTRAL-COM_FirmwareDownloadURL WITH CDL URL VALUE.");
			LOGGER.info("STEP :  " + stepNumber + " : EXPECTED: FIRMWARE DOWNLOAD URL MUST BE SET & LOGGED.");
			LOGGER.info("#######################################################################################");

			firmwareFileName = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device);
			LOGGER.info("The current build name is " + firmwareFileName);

			if (DeviceModeHandler.isDSLDevice(device)
					&& firmwareFileName.contains(
							AutomaticsTapApi.getSTBPropsValue(BroadBandPropertyKeyConstants.STRING_BUILDNAME_PART))
					&& firmwareFileName.contains("sprint")) {
				logFile = BroadBandCdlConstants.FWUPGRADEMANAGER_LOG_FILE_NAME;
			} else {

				logFile = BroadBandCommonUtils.getLogFileNameOnFirmwareDownload(device);
			}
			errorMessage = "Unable to set the Firmware Download URL using WebPA.";
			result = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandCdlConstants.WEBPA_PARAM_CODE_DOWNLOAD_URL, BroadBandTestConstants.CONSTANT_0,
					codeDownloadUrl);
			if (result) {
				tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
				searchResponse = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
						BroadBandCdlConstants.LOG_MESSAGE_CDL_URL + codeDownloadUrl, logFile);
				result = CommonMethods.isNotNull(searchResponse);
				errorMessage = "Unable to verify the log message after setting the Code Download URL using webpa.";
			}
			if (result) {
				LOGGER.info("STEP " + stepNumber
						+ ": ACTUAL : FIRMWARE DOWNLOAD URL SET USING WEBPA AND LOG MESSAGE VERIFIED.");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, true);

			/**
			 * S2) Verify setting the appropriate value for parameter:
			 * Device.DeviceInfo.X_RDKCENTRAL-COM_FirmwareToDownload
			 */
			stepNumber++;
			step = "S" + stepNumber;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP :  " + stepNumber + " : DESCRIPTION : VERIFY SETTING THE FIRMWARE DOWNLOAD IMAGE NAME.");
			LOGGER.info("STEP :  " + stepNumber
					+ " : ACTION : EXECUTE SET COMMAND ON Device.DeviceInfo.X_RDKCENTRAL-COM_FirmwareToDownload WITH NEW IMAGE VERSION");
			LOGGER.info("STEP :  " + stepNumber
					+ " : EXPECTED: FIRMWARE DOWNLOAD DOWNLOAD IMAGE NAME MUST BE SET & LOGGED.");
			LOGGER.info("#######################################################################################");
			errorMessage = "Unable to set the Firmware Download Image Name using WebPA.";
			result = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandCdlConstants.WEBPA_PARAM_CODE_DOWNLOAD_IMAGE_NAME, BroadBandTestConstants.CONSTANT_0,
					latestCdlImageBin);
			if (result) {
				tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
				searchResponse = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
						BroadBandCdlConstants.LOG_MESSAGE_CDL_IMAGE_NAME + latestCdlImageBin, logFile);
				result = CommonMethods.isNotNull(searchResponse);
				errorMessage = "Unable to verify the log message after setting the Image Name using webpa.";
			}
			if (result) {
				LOGGER.info("STEP " + stepNumber
						+ ": ACTUAL : FIRMWARE DOWNLOAD IMAGE NAME SET USING WEBPA AND LOG MESSAGE VERIFIED");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, true);

			/**
			 * S3) Verify triggering the code download using parameter:
			 * Device.DeviceInfo.X_RDKCENTRAL-COM_FirmwareDownloadNow
			 */
			stepNumber++;
			step = "S" + stepNumber;
			result = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info(
					"STEP :  " + stepNumber + " : DESCRIPTION : VERIFY TRIGGERING THE FIRMWARE DOWNLOAD USING WEBPA.");
			LOGGER.info("STEP :  " + stepNumber
					+ " : ACTION : EXECUTE SET COMMAND ON Device.DeviceInfo.X_RDKCENTRAL-COM_FirmwareDownloadNow WITH VALUE true");
			LOGGER.info("STEP :  " + stepNumber + " : EXPECTED: FIRMWARE DOWNLOAD PARAMETER MUST BE SET.");
			LOGGER.info("#######################################################################################");
			errorMessage = "Unable to trigger the code download using WebPA.";
			result = BroadBandWiFiUtils.setWebPaParams(device, BroadBandCdlConstants.WEBPA_PARAM_TRIGGER_CDL,
					BroadBandTestConstants.TRUE, BroadBandTestConstants.CONSTANT_3);
			if (result) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL :FIRMWARE DOWNLOAD PARAMETER SET SUCCESSFULLY.");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, true);

			/**
			 * S4) Verify the code download status is 'In Progress'.
			 */
			stepNumber++;
			step = "S" + stepNumber;
			result = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP :  " + stepNumber
					+ " : DESCRIPTION : VERIFY THE CODE DOWNLOAD STATUS AFTER SUCCESSFUL CDL TRIGGER.");
			LOGGER.info("STEP :  " + stepNumber
					+ " : ACTION : VERIFY THE CODE DOWNLOAD STATUS BY EXECUTING WEBPA GET COMMAND -Device.DeviceInfo.X_RDKCENTRAL-COM_FirmwareDownloadStatus");
			LOGGER.info("STEP :  " + stepNumber + " : EXPECTED: CODE DOWNLOAD STATUS MUST BE 'In Progress'.");
			LOGGER.info("#######################################################################################");
			errorMessage = "'In progress'  Code Download Status is not observed for webpa param Device.DeviceInfo.X_RDKCENTRAL-COM_FirmwareDownloadStatus";
			tapEnv.waitTill(BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
			ArrayList<String> statusesOfFirmwareDownloadParam = BroadBandCommonUtils
					.captureFirmWareDownloadStatusesDuringCdl(tapEnv, device);
			if (statusesOfFirmwareDownloadParam != null) {
				result = statusesOfFirmwareDownloadParam.contains(BroadBandCdlConstants.CDL_STATUS_IN_PROGRESS);
			} else {
				errorMessage = "Not able to capture the Code Download Statuses using WEBPA";
			}
			if (result) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL :CODE DOWNLOAD STATUS IS 'In Progress'.");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);
			/**
			 * S5) Verify the code download status is 'Completed'.
			 */
			stepNumber++;
			step = "S" + stepNumber;
			result = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP :  " + stepNumber
					+ " : DESCRIPTION : VERIFY THE CODE DOWNLOAD STATUS AFTER SUCCESSFUL CDL TRIGGER.");
			LOGGER.info("STEP :  " + stepNumber
					+ " : ACTION : VERIFY THE CODE DOWNLOAD STATUS BY EXECUTING WEBPA GET COMMAND -Device.DeviceInfo.X_RDKCENTRAL-COM_FirmwareDownloadStatus");
			LOGGER.info("STEP :  " + stepNumber + " : EXPECTED: CODE DOWNLOAD STATUS MUST BE 'Completed'.");
			LOGGER.info("#######################################################################################");
			errorMessage = "'Completed'  Code Download Status is not observed for webpa param Device.DeviceInfo.X_RDKCENTRAL-COM_FirmwareDownloadStatus";
			if (statusesOfFirmwareDownloadParam != null) {
				result = statusesOfFirmwareDownloadParam.contains(BroadBandCdlConstants.CDL_STATUS_COMPLETED);
			} else {
				errorMessage = "Not able to capture the Code Download Statuses using WEBPA";
			}
			if (result) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL :CODE DOWNLOAD STATUS IS 'Completed'.");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

			/**
			 * 6) Verify the CDL is triggered successfully.
			 */
			stepNumber++;
			step = "S" + stepNumber;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP :  " + stepNumber + " : DESCRIPTION : VERIFY THE CDL IS TRIGGERED SUCCESSFULLY.");
			LOGGER.info("STEP :  " + stepNumber
					+ " : ACTION : EXECUTE grep -i \"Image downloading triggered successfully\" /rdklogs/logs/CMlog.txt.0");
			LOGGER.info("STEP :  " + stepNumber + " : EXPECTED: CDL MUST BE TRIGGERED SUCCESSFULLY.");
			LOGGER.info("#######################################################################################");
			tapEnv.waitTill(BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
			searchResponse = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandCdlConstants.LOG_MESSAGE_CDL_TRIGGERED, logFile);
			result = CommonMethods.isNotNull(searchResponse);
			errorMessage = "Unable to verify the CDL is triggered successfully.";
			if (result) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL :CDL TRIGGERED SUCCESSFULLY.");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

			/**
			 * S7) Verify the device reboots after successful code download.
			 */
			stepNumber++;
			step = "S" + stepNumber;
			result = false;
			pollDuration = BroadBandTestConstants.FIVE_MINUTES;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP :  " + stepNumber
					+ " : DESCRIPTION : VERIFY THE DEVICE REBOOTS AFTER SUCCESSFUL CODE DOWNLOAD.");
			LOGGER.info("STEP :  " + stepNumber + " : ACTION : CHECK DEVICE IS GOING FOR REBOOT");
			LOGGER.info("STEP :  " + stepNumber + " : EXPECTED: DEVICE MUST REBOOT AFTER SUCCESSFUL CODE DOWNLOAD.");
			LOGGER.info("#######################################################################################");
			errorMessage = "Unable to verify the device reboot after successful code download.";
			long delayDuration = BroadBandTestConstants.ONE_MINUTE_IN_MILLIS;
			result = BroadBandCommonUtils.isRdkbDeviceAccessible(tapEnv, device, delayDuration, pollDuration, false);
			if (result) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL :DEVICE REBOOTS AFTER SUCCESSFUL CODE DOWNLOAD.");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, true);

			/**
			 * S8) Verify the device is accessible after successful code download & reboot.
			 */
			stepNumber++;
			step = "S" + stepNumber;
			result = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP :  " + stepNumber
					+ " : DESCRIPTION : VERIFY THE DEVICE IS ACCESSIBLE AFTER SUCCESSFUL CODE DOWNLOAD & REBOOT.");
			LOGGER.info("STEP :  " + stepNumber + " : ACTION : VERIFY THE DEVICE IS ACCESSIBLE");
			LOGGER.info("STEP :  " + stepNumber
					+ " : EXPECTED: DEVICE MUST BE ACCESSIBLE AFTER SUCCESSFUL CODE DOWNLOAD & REBOOT.");
			LOGGER.info("#######################################################################################");
			errorMessage = "Unable to verify the device is accessible after CDL.";
			pollDuration = BroadBandTestConstants.FIFTEEN_MINUTES_IN_MILLIS;
			startTime = System.currentTimeMillis();
			do {
				LOGGER.info("GOING TO WAIT FOR 1 MINUTE.");
				tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
				result = CommonMethods.isSTBAccessible(device);
			} while ((System.currentTimeMillis() - startTime) < pollDuration && !result);
			if (result) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL :DEVICE IS ACCESSIBLE AFTER SUCCESSFUL CODE DOWNLOAD.");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, true);

			/**
			 * S9) Verify the image is flashed successfully.
			 */
			stepNumber++;
			step = "S" + stepNumber;
			result = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP :  " + stepNumber + " : DESCRIPTION : VERIFY THE IMAGE IS FLASHED SUCCESSFULLY.");
			LOGGER.info("STEP :  " + stepNumber
					+ " : ACTION : VERIFY THE IMAGE NAME BY EXECUTING WEBPA GET COMMAND- Device.DeviceInfo.X_CISCO_COM_FirmwareName");
			LOGGER.info("STEP :  " + stepNumber + " : EXPECTED: IMAGE MUST BE FLASHED SUCCESSFULLY.");
			LOGGER.info("#######################################################################################");
			errorMessage = "Unable to verify the image is flashed successfully.";
			pollDuration = BroadBandTestConstants.THREE_MINUTE_IN_MILLIS;
			startTime = System.currentTimeMillis();
			String tempFirmwareVersion = null;

			// add signed extension to the image
			String buildExtension = BroadBandTestConstants.SIGNED_BUILD_IMAGE_EXTENSION;
			try {
				buildExtension = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
						BroadBandPropertyKeyConstants.PROP_KEY_SIGNED_EXTENSION);
			} catch (Exception e) {
				buildExtension = BroadBandTestConstants.SIGNED_BUILD_IMAGE_EXTENSION;
				LOGGER.info("No platform dependent extensions are mentioned in  automatics properties");
			}
			String latestCdlImageWithBuildExtn = latestCdlImage + buildExtension;
			LOGGER.info(" ImageName with Extension : " + latestCdlImageWithBuildExtn);

			do {
				tempFirmwareVersion = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device);

				LOGGER.info("tempFirmwareVersion is " + tempFirmwareVersion.trim());
				LOGGER.info("latestCdlImageWithBuildExtn is " + latestCdlImageWithBuildExtn);

				result = CommonMethods.isNotNull(tempFirmwareVersion)
						&& latestCdlImageWithBuildExtn.equalsIgnoreCase(tempFirmwareVersion.trim());
			} while ((System.currentTimeMillis() - startTime) < pollDuration && !result
					&& BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS));
			if (result) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL :IMAGE IS FLASHED SUCCESSFULLY ON THE DEVICE.");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, true);
		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error(
					"TC-RDKB-CDL-5502 : EXCEPTION OCCURRED WHILE VALIDATING THE HTTP TR-181 DOWNLOAD: " + errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, step, result, errorMessage, true);
		} finally {
			result = false;
			int postConditionStepNum = 0;
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			/**
			 * POST CONDITION : VERIFY THAT DEVICE IS UPGRADED WITH THE PREVIOUS FIRMWARE
			 * VERSION.
			 */
			result = false;
			String imageAfterTriggering = "";
			errorMessage = "Device is not accessible even after waiting for 10 mins.";
			String successMessage = "";
			postConditionStepNum++;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("POST-CONDITION " + postConditionStepNum
					+ ": DESCRIPTION : Verify reverting device to original image.");
			LOGGER.info("POST-CONDITION " + postConditionStepNum
					+ ": ACTION : Flash the original build on the device using HTTP/ TR-181.");
			LOGGER.info("POST-CONDITION " + postConditionStepNum
					+ ": EXPECTED : Device should be upgraded to original image.");
			LOGGER.info("**********************************************************************************");
			try {
				if (CommonMethods.isSTBAccessible(tapEnv, device, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
						BroadBandTestConstants.CONSTANT_10)) {
					errorMessage = "Unable to get current firmware version.";
					imageAfterTriggering = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device);
					if (CommonMethods.isNotNull(imageAfterTriggering)
							&& CommonMethods.isNotNull(currentFirmwareVersion)) {
						if (!imageAfterTriggering.equals(currentFirmwareVersion)) {
							result = BroadBandCodeDownloadUtils.upgradeDeviceWithGivenFirmwareVersion(device, tapEnv,
									currentFirmwareVersion);
							if (!result) {
								result = BroadBandCodeDownloadUtils.triggerPreviousCodeDownload(device, tapEnv,
										currentFirmwareVersion);
							}
						} else {
							successMessage = "Device Build hasn't changed so need to revert the device image.";
							result = true;
						}
					}
				}
			} catch (Exception e) {
				errorMessage = "Exception occured during reverting the device back to original image." + errorMessage
						+ e.getMessage();
			}
			if (result) {
				LOGGER.info("POST-CONDITION " + postConditionStepNum + ": ACTUAL : " + successMessage);
			} else {
				LOGGER.error("POST-CONDITION " + postConditionStepNum + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-CDL-5502");
	}
}
