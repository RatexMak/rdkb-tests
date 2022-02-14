package com.automatics.rdkb.tests.reboot;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.AutomaticsConstants;
import com.automatics.constants.DataProviderConstants;
import com.automatics.constants.LinuxCommandConstants;
import com.automatics.constants.XconfConstants;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.AutomaticsPropertyUtility;
import com.automatics.utils.CommonMethods;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandCdlConstants;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandPropertyKeyConstants;
import com.automatics.rdkb.constants.BroadBandTelemetryConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.RDKBTestConstants;
import com.automatics.rdkb.constants.WebPaParamConstants.WebPaDataTypes;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandPostConditionUtils;
import com.automatics.rdkb.utils.BroadBandPreConditionUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.cdl.BroadBandCodeDownloadUtils;
import com.automatics.rdkb.utils.cdl.BroadBandXconfCdlUtils;
import com.automatics.rdkb.utils.cdl.FirmwareDownloadUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;

/**
 * 
 * Reboot Primitives specific test cases for RDK B boxes
 * 
 * @author Sumathi Gunasekaran
 * 
 *
 */
public class BroadBandRebootPrimitivesTests extends AutomaticsTestBase {

	/**
	 *
	 * Test Case: Verify abort reboot after reboot initiates
	 *
	 * <p>
	 * STEPS:
	 * </p>
	 * <ol>
	 * <li>STEP 1: Set and verify defer firmware download reboot value through webpa
	 * command other than default value</li>
	 * <li>STEP 2: Configure XCONF server with required configuration using REST
	 * API</li>
	 * <li>STEP 3: Verify XCONF firmware download triggered successfully</li>
	 * <li>STEP 4: Verify firmware download is successful</li>
	 * <li>STEP 5: Verify defer firmware download reboot value in log message</li>
	 * <li>STEP 6: Set and verify abort reboot value through webpa command other
	 * than default value</li>
	 * </ol>
	 *
	 * @author Sumathi Gunasekaran
	 * 
	 * @param device {@link Dut}
	 * @Refactor Sruthi Santhosh
	 */
	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
	@TestDetails(testUID = "TC-RDKB-REBOOT-1015")
	public void testToVerifyAbortRebootInReboot(Dut device) {
		// Variable to store testcaseID
		String testCaseId = "TC-RDKB-REBOOT-015";
		// Variable to store step number
		String stepNumber = "s1";
		// variable to store status
		boolean status = false;
		// Variable to store errorMessage
		String errorMessage = null;
		// Variable to store current image name
		String currentImageName = null;
		// Variable to store imageNameForCdl
		String imageNameForCdl = null;
		// Variable to store expectedLogMessage
		String expectedLogMessage = null;
		// Variable to store cdlLogsForValidation
		String cdlLogsForValidation = null;
		// Variable to store imageName for post condition cdl
		String imageToCDLPostCondition = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device);
		try {
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			/**
			 * PRE-CONDITION 1 : Pre-Condition method to disable code big first.
			 */
			BroadBandPreConditionUtils.executePreConditionToDisableCodeBigFirst(device, tapEnv,
					BroadBandTestConstants.CONSTANT_1);
			LOGGER.info("#######################################################################################");

			// STEP 1: Set and verify defer firmware download reboot value through webpa
			// command other than default
			// value
			LOGGER.info("************************************************************");
			LOGGER.info(
					"STEP 1: Set and verify defer firmware download reboot value through webpa command other than default value");
			LOGGER.info("EXPECTED: Webpa command should execute successfully and return value");
			LOGGER.info("************************************************************");
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEFER_FIRMWARE_DOWNLOAD_REBOOT,
					BroadBandTestConstants.CONSTANT_2, BroadBandTestConstants.STRING_10);
			LOGGER.info("Is defer firmware download reboot value is set other than default: " + status);
			errorMessage = "Failed to Set value for WebPa parameter:"
					+ BroadBandWebPaConstants.WEBPA_PARAM_DEFER_FIRMWARE_DOWNLOAD_REBOOT + "with Value:"
					+ BroadBandTestConstants.STRING_10;
			LOGGER.info(
					"STEP : ACTUAL: " + (status
							? "defer firmware download reboot value is set successfully with "
									+ BroadBandWebPaConstants.WEBPA_PARAM_DEFER_FIRMWARE_DOWNLOAD_REBOOT
							: errorMessage));
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

			// STEP 2: Configure XCONF server with required configuration using REST API
			stepNumber = "s2";
			status = false;
			errorMessage = null;
			LOGGER.info("************************************************************");
			LOGGER.info("STEP 2: Configure XCONF server with required configuration using REST API");
			LOGGER.info(
					"EXPECTED: Should be able to configure the code download configuration using XCONF Rest API and swupdate.conf should updated with xconf url");
			LOGGER.info("************************************************************");
			currentImageName = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device);
			imageNameForCdl = tapEnv.getLatestBuildImageVersionForCdlTrigger(device, false);
			LOGGER.info("LATEST FIRMWARE VERSION: " + imageNameForCdl);
			if (CommonMethods.isNull(imageNameForCdl)) {
				LOGGER.info(
						" GA image obtained from deployed version service is null. Hence getting the image from property file ");
				imageNameForCdl = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
						BroadBandPropertyKeyConstants.PARTIAL_PROPERTY_KEY_FOR_GA_IMAGE);
				LOGGER.info("Latest Firmware version from property file: " + imageNameForCdl);
			}
			errorMessage = "Unable to retrieve latest image name for RDKB devices";
			LOGGER.info("Latest Image to CDL is: " + imageNameForCdl);
			if (CommonMethods.isNotNull(imageNameForCdl)) {
				BroadBandXconfCdlUtils.configureRdkbDeviceForXconfCdl(tapEnv, device, imageNameForCdl, false,
						BroadBandTestConstants.FIRMWARE_DOWNLOAD_PROTOCOL_HTTP);
				status = true;
			}
			LOGGER.info("STEP : ACTUAL: "
					+ (status ? "Successfully configured XCONF server with required configuration using REST API"
							: errorMessage));
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
			// STEP 3 Reboot the device and check whether device comes up properly.
			stepNumber = "s3";
			status = false;
			errorMessage = null;
			LOGGER.info("************************************************************");
			LOGGER.info("STEP 3:  Reboot the device to initiate firmware upgrade.");
			LOGGER.info("EXPECTED: STB should reboot.");
			LOGGER.info("************************************************************");
			status = CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv);
			errorMessage = "Unable to connect to RDKB device after reboot";
			LOGGER.info("STEP : ACTUAL: "
					+ (status ? "RDKB device is rebooted successfully to initiate firmware upgrade." : errorMessage));
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			// STEP 4: Verify XCONF firmware download triggered successfully
			stepNumber = "s4";
			status = false;
			errorMessage = null;
			LOGGER.info("************************************************************");
			LOGGER.info("STEP 4:  Verify XCONF firmware download triggered successfully");
			LOGGER.info("EXPECTED: Code download triggered message should be logged in Xconf log");
			LOGGER.info("************************************************************");
			cdlLogsForValidation = FirmwareDownloadUtils.getCdlLogsForValidation(tapEnv, device);
			currentImageName = BroadBandCommonUtils
					.removeDifferentSignedExtensionsInRequestedBuildName(currentImageName);
			imageNameForCdl = BroadBandCommonUtils.removeDifferentSignedExtensionsInRequestedBuildName(imageNameForCdl);
			expectedLogMessage = BroadBandCdlConstants.XCONF_CODE_DOWNLOAD_MESSAGE
					.replaceAll(BroadBandCdlConstants.CONFIGURATION_CURRENT_IMAGE_NAME, currentImageName)
					.replaceAll(BroadBandCdlConstants.CONFIGURATION_REQUESTED_IMAGE_NAME, imageNameForCdl);
			status = CommonMethods.patternMatcher(cdlLogsForValidation.toLowerCase(), expectedLogMessage.toLowerCase());

			errorMessage = "Unable to find the XCONF HTTP CDL accepted message '" + expectedLogMessage + "' in "
					+ BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0;
			LOGGER.info("STEP :ACTUAL: "
					+ (status
							? "Successfully validated the XCONF HTTP CDL accepted message in "
									+ BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0
							: errorMessage));
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

			// STEP 5: Verify firmware download is successful
			stepNumber = "s5";
			status = false;
			errorMessage = null;
			LOGGER.info("************************************************************");
			LOGGER.info("STEP 5: Verify firmware download is successful");
			LOGGER.info("EXPECTED: Xconf log should have HTTP download Successful log message");
			LOGGER.info("************************************************************");
			expectedLogMessage = FirmwareDownloadUtils.getCdlDownloadSuccessLog(device);
			status = CommonUtils.isGivenStringAvailableInCommandOutput(cdlLogsForValidation.toLowerCase(),
					expectedLogMessage.toLowerCase());
			errorMessage = "Unable to retrieve Code download completed message from " + expectedLogMessage + " in "
					+ BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0;

			LOGGER.info("STEP : ACTUAL: "
					+ (status
							? "Successfully validated the XCONF HTTP CDL completed message in "
									+ BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0
							: errorMessage));
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

			// STEP 6: Set and verify abort reboot value through webpa command other than
			// default value
			stepNumber = "s6";
			status = false;
			errorMessage = null;
			LOGGER.info("************************************************************");
			LOGGER.info("STEP 6: Verify abort is sent through webPA after reboot begins");
			LOGGER.info("EXPECTED: Since the box has gone for reboot, the device does not respond anything.");
			LOGGER.info("************************************************************");
			tapEnv.executeCommandUsingSsh(device, LinuxCommandConstants.CMD_REBOOT);
			long startTime = System.currentTimeMillis();
			do {
				if (!CommonMethods.isSTBAccessible(device)) {
					LOGGER.info("STB is not accessible!. Reboot has initiated successfully");
					status = !BroadBandWebPaUtils.verifyWebPaValueAfterDuration(device, tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_ABORT_REBOOT, BroadBandTestConstants.CONSTANT_3,
							BroadBandTestConstants.TRUE, BroadBandTestConstants.CONSTANT_0);
					errorMessage = "Value is set successfully for WebPa parameter when box has initiated reboot";
					break;
				}
			} while ((System.currentTimeMillis() - startTime) < AutomaticsConstants.FIVE_MINUTES && !status
					&& BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS));
			LOGGER.info("ACTUAL: " + (status ? "Device intiated Reboot before sending abort request" : errorMessage));
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("Exception Occurred while Aborting the reboot where reboot is already initiated in device:"
					+ errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
					true);
		} finally {
			// POST CONDITION
			// Since box has gone for Reboot, waiting for the box to come up.
			CommonMethods.waitForEstbIpAcquisition(tapEnv, device);
			// Reverting back to the default image in post condition
			LOGGER.info("Reverting back to the default image:");
			LOGGER.info("Image for CDL is:" + imageToCDLPostCondition);
			FirmwareDownloadUtils.deleteSoftwareUpdateConfigurationFile(tapEnv, device);

			status = FirmwareDownloadUtils.triggerCdlUsingTr181OrTftp(tapEnv, device, imageToCDLPostCondition);
			errorMessage = "Failed to downgrade image to default image";
			FirmwareDownloadUtils.deleteSoftwareUpdateConfigurationFile(tapEnv, device);
			LOGGER.info("POST CONDITION: ACTUAL: "
					+ (status ? "Successfully reverted back to the default image" : errorMessage));
		}
	}
    /**
    *
    * Test Case # 1: Verify that device supports a reboot command that contains the component, reboot source and delay
    *
    * <p>
    * STEPS:
    * </p>
    * <ol>
    * <li>STEP 1 :Verify device supports a reboot command that has default components, reboot source and delay</li>
    * <li>STEP 2 :Verify device supports a reboot command with all device components and default delay and Reboot
    * reason as WebPA-reboot</li>
    * <li>STEP 3 :Verify device supports a reboot command with all device components and default delay and Reboot
    * reason as xFi-reboot</li>
    * <li>STEP 4 :Verify device supports a reboot command with all device components and default delay and Reboot
    * reason as CSR-reboot</li>
    * <li>STEP 5 :Verify device supports a reboot command with all device components and default delay and Reboot
    * reason as unknown</li>
    * <li>STEP 6 :Verify device supports a reboot command with all device components and 10 seconds delay and default
    * reboot reason</li>
    * <li>STEP 7 :Verify device supports a reboot command with device component as Wifi and default reboot reason</li>
    * <li>STEP 8 :Verify device supports a reboot command with device component as Router and 10 seconds delay and
    * default reboot reason</li>
    * </ol>
    *
    * @author Sumathi Gunasekaran
    * @Refactor Rakesh C N
    * @param settop
    *            {@link Dut}
    * @throws Exception
    */

   @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
   @TestDetails(testUID = "TC-RDKB-REBOOT-1011")
   public void testToVerifyRebootReason(Dut settop) {
	// Variable declaration starts
	String testCaseId = "TC-RDKB-REBOOT-011";
	String errorMessage = null;
	boolean status = false;
	int stepNumber = 1;
	String step = "S" + stepNumber;
	String webPAValueToVerify = null;
	// Variable declaration ends
	LOGGER.info("**********************************************************************************");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-REBOOT-1011");
	LOGGER.info("TEST DESCRIPTION: Perform reboot with device component, delay and reboot reason");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info(
		"Step 1 : Verify device supports a reboot command that has default components, reboot source and delay.");
	LOGGER.info(
		"Step 2 : Verify device supports a reboot command with all device components and default delay and Reboot reason as WebPA-reboot");
	LOGGER.info(
		"Step 3 : Verify device supports a reboot command with all device components and default delay and Reboot reason as CSR-reboot.");
	LOGGER.info(
		"Step 4 : Verify device supports a reboot command with all device components and default delay and Reboot reason as unknown");
	LOGGER.info(
		"Step 5 : Verify device supports a reboot command with all device components and 10 seconds delay and default reboot reason");
	LOGGER.info(
		"Step 6 : Verify device supports a reboot command with device component as Wifi and default reboot reason");
	LOGGER.info(
		"Step 7 : Verify device supports a reboot command with device component as Router and 10 seconds delay and default reboot reason.");
	LOGGER.info("**********************************************************************************");
	try {
	    /**
	     * STEP 1 : VERIFY DEVICE SUPPORTS A REBOOT COMMAND THAT HAS DEFAULT COMPONENTS, REBOOT SOURCE AND DELAY
	     */
	    executeTestStepToPerformDifferentReboot(settop, tapEnv, testCaseId, stepNumber,
		    BroadBandTestConstants.DEFAULT_DELAY,
		    BroadBandTelemetryConstants.LAST_REBOOT_REASON_FOR_WEBPA_REBOOT, false);

	    /**
	     * STEP 2 : VERIFY DEVICE SUPPORTS A REBOOT COMMAND WITH ALL DEVICE COMPONENTS AND DEFAULT DELAY AND REBOOT
	     * REASON AS WEBPA-REBOOT
	     */
	    stepNumber++;
	    webPAValueToVerify = BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.DEVICE,
		    BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandTestConstants.SOURCE,
		    BroadBandTelemetryConstants.LAST_REBOOT_REASON_FOR_WEBPA_REBOOT);
	        executeTestStepToPerformDifferentReboot(settop, tapEnv, testCaseId, stepNumber, webPAValueToVerify,
		    BroadBandTelemetryConstants.LAST_REBOOT_REASON_FOR_WEBPA_REBOOT, false);

	    /**
	     * STEP 3 : VERIFY DEVICE SUPPORTS A REBOOT COMMAND WITH ALL DEVICE COMPONENTS AND DEFAULT DELAY AND REBOOT
	     * REASON AS CSR-REBOOT
	     */
	    stepNumber++;
	    webPAValueToVerify = BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.DEVICE,
		    BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandTestConstants.SOURCE,
		    BroadBandTestConstants.CSR_REBOOT_REASON);
	    executeTestStepToPerformDifferentReboot(settop, tapEnv, testCaseId, stepNumber, webPAValueToVerify,
		    BroadBandTestConstants.CSR_REBOOT_REASON, false);

	    /**
	     * STEP 4 : VERIFY DEVICE SUPPORTS A REBOOT COMMAND WITH ALL DEVICE COMPONENTS AND DEFAULT DELAY AND REBOOT
	     * REASON AS UNKNOWN
	     */
	    stepNumber++;
	    webPAValueToVerify = BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.DEVICE,
		    BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandTestConstants.SOURCE,
		    BroadBandTestConstants.UNKNOWN_REBOOT_REASON);
	    executeTestStepToPerformDifferentReboot(settop, tapEnv, testCaseId, stepNumber, webPAValueToVerify,
		    BroadBandTestConstants.UNKNOWN_REBOOT_REASON, false);

	    /**
	     * STEP 5 : VERIFY DEVICE SUPPORTS A REBOOT COMMAND WITH ALL DEVICE COMPONENTS AND 10 SECONDS DELAY AND
	     * DEFAULT REBOOT REASON
	     */
	    stepNumber++;
	    webPAValueToVerify = BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.DEVICE,
		    BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandTestConstants.DELAY);
	    executeTestStepToPerformDifferentReboot(settop, tapEnv, testCaseId, stepNumber, webPAValueToVerify,
		    BroadBandTelemetryConstants.LAST_REBOOT_REASON_FOR_WEBPA_REBOOT, false);

	    /**
	     * STEP 6 : VERIFY DEVICE SUPPORTS A REBOOT COMMAND WITH DEVICE COMPONENT AS WIFI AND DEFAULT REBOOT REASON
	     */
	    stepNumber++;
	    executeTestStepToPerformDifferentReboot(settop, tapEnv, testCaseId, stepNumber,
		    BroadBandTestConstants.WIFI_WEBPA_REBOOT, BroadBandTraceConstants.LOG_MESSAGE_WIFI_REBOOT_MESSAGE,
		    true);

	    /**
	     * STEP 7 : VERIFY DEVICE SUPPORTS A REBOOT COMMAND WITH DEVICE COMPONENT AS ROUTER AND DEFAULT REBOOT
	     * REASON
	     */
	    stepNumber++;
	    executeTestStepToPerformDifferentReboot(settop, tapEnv, testCaseId, stepNumber,
		    BroadBandTestConstants.STRING_ROUTER, BroadBandTraceConstants.LOG_MESSAGE_ROUTER_REBOOT_MESSAGE,
		    true);
	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("Exception Occurred while Validating Reboot Parameter:" + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, settop, testCaseId, step, status, errorMessage, true);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-REBOOT-1011");
   }
   
   /**
    * Test step method used to verify MoCA page greyed out status in MSO/LAN GUI page
    * 
    * @param settop
    *            instance of{@link Dut}
    * @param tapEnv
    *            instance of {@link AutomaticsTapApi}
    * @param testCaseId
    *            Test case ID
    * @param stepNumber
    *            Step Number
    * @param webPASetValue
    *            Value to be to set for reboot
    * @param rebootReason
    *            Reboot reason to verify
    * @param iswifiOrRouterReboot
    * 
    */
   public static void executeTestStepToPerformDifferentReboot(Dut settop, AutomaticsTapApi tapEnv, String testCaseId,
	    int stepNumber, String webPASetValue, String rebootReason, boolean iswifiOrRouterReboot) {
	/**
	 * STEP : VERIFY DIFFERENT REBOOT REASON
	 */
	String stepNum = "S" + stepNumber;
	boolean status = false;
	String errorMessage = null;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : Verify device supports reboot command with "
		+ (iswifiOrRouterReboot ? "device component as " + webPASetValue : "all device components")
		+ " and Reboot reason as " + rebootReason);
	LOGGER.info("STEP " + stepNumber
		+ " : ACTION : EXECUTE WEBPA COMMAND : Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.RPC.RebootDevice with value as "
		+ webPASetValue);
	LOGGER.info("STEP " + stepNumber + " : EXPECTED : " + (iswifiOrRouterReboot
		? "The Log message should be " + rebootReason : "The Last Reboot Reason should be " + rebootReason));
	LOGGER.info("**********************************************************************************");
	errorMessage = "Failed to set value through WebPA command for parameter : Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.RPC.RebootDevice with value as "
		+ webPASetValue;
	if (DeviceModeHandler.isDSLDevice(settop)
		&& webPASetValue.equals(BroadBandTestConstants.STRING_ROUTER)) {
	    LOGGER.info("The device Model is DSLDevice");
	    LOGGER.info("This step is skipped as it is not applicable for DSL devices.");
	    tapEnv.updateExecutionForAllStatus(settop, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
		    "This test step not applicable", false);
	} else {
	    if (BroadBandWiFiUtils.setWebPaParams(settop, BroadBandWebPaConstants.WEBPA_PARAM_REBOOT_DEVICE,
		    webPASetValue, BroadBandTestConstants.CONSTANT_0)) {
		if (iswifiOrRouterReboot) {
		    long startTime = System.currentTimeMillis();
		    do {
			status = CommonUtils.isNotEmptyOrNull(BroadBandCommonUtils.searchLogFiles(tapEnv, settop,
				rebootReason, BroadBandTestConstants.COMMAND_NTP_LOG_FILE));
		    } while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.THREE_MINUTE_IN_MILLIS
			    && !status && BroadBandCommonUtils.hasWaitForDuration(tapEnv,
				    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
		    if (!status) {
			status = CommonUtils.validateTraceLog(tapEnv, settop, rebootReason,
				BroadBandTestConstants.ONE_MINUTE_IN_MILLIS, true);
		    }
		    errorMessage = "Failed to retrieve log message from :"
			    + BroadBandTestConstants.COMMAND_NTP_LOG_FILE;
		} else {
		    status = FirmwareDownloadUtils.verifyGivenRebootResonFromWebPaCommand(tapEnv, settop, rebootReason);
		    errorMessage = "Failed to Verify Last Reboot Reason through WebPA Command for:" + rebootReason;
		}
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber + " : ACTUAL : Successfully Verified "
			+ (iswifiOrRouterReboot
				? "reboot message from log file" + BroadBandTestConstants.COMMAND_NTP_LOG_FILE
				: "last reboot reason through webpa command for " + rebootReason));
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(settop, testCaseId, stepNum, status, errorMessage, false);
	}
   }
   
   /**
   *
   * Test Case # 1: Verify device gives response as invalid request when there is no reboot is pending and abort
   * reboot command is sent
   *
   * <p>
   * STEPS:
   * </p>
   * <ol>
   * <li>PRE-CONDITION 1 : Verify the current image name and image name for cdl</li>
   * <li>PRE-CONDITION 2 : Reboot the device and wait for ip accusition</li>
   * <li>PRE-CONDITION 3 : DISABLE CODEBIG BY USING WEBPA<li>
   * <li>STEP 1: Verify default value for abort reboot is false</li>
   * <li>STEP 2: Execute WebPa command to abort the reboot</li>
   * <li>STEP 3: Verify invalid request is received as response for reboot abort</li>
   * <li>STEP 4: Configure XCONF server with required configuration using REST API</li>
   * <li>STEP 5: Reboot the device to initiate firmware upgrade</li>
   * <li>STEP 6: Verify XCONF firmware download triggered successfully</li>
   * <li>STEP 7: Verify firmware download is successful</li>
   * <li>STEP 8: Set and verify abort reboot value as true through WebPa command</li>
   * <li>STEP 9: Verify 290 sleep process running using ps</li>
   * <li>STEP 10: Verify abort count value in XCONF log file</li>
   * <li>STEP 11: Set and verify abort reboot value as true through WebPa command</li>
   * <li>STEP 12: Verify 290 sleep process running using ps</li>
   * <li>STEP 13: Verify abort count value in XCONF log file</li>
   * <li>STEP 14: Set and verify abort reboot value as true through WebPa command</li>
   * <li>STEP 15: Verify 290 sleep process running using ps</li>
   * <li>STEP 16: Verify abort count value in XCONF log file</li>
   * <li>STEP 17: Set and verify abort reboot value as true through WebPa command</li>
   * <li>STEP 18: Verify 290 sleep process running using ps</li>
   * <li>STEP 19: Verify abort count value in XCONF log file</li>
   * <li>STEP 20: Set and verify abort reboot value as true through WebPa command</li>
   * <li>STEP 21: Verify 290 sleep process running using ps</li>
   * <li>STEP 22: Verify abort count value in XCONF log file</li>
   * <li>STEP 23: Verify abort count value reached maximum value</li>
   * <li>STEP 24: Verify device rebooting after reboot count reaches maximum limit</li>
   * <li>POST-CONDITION 1 : Verify that device is upgraded with the previous firmware version</li>
   * </ol>
   *
   * @author Sumathi Gunasekaran 
   * 
   * @author ArunKumar Jayachandran,Muthukumar
   * 
   * @param device
   *            {@link Dut}
   * @Refactor Sruthi Santhosh
   */
  @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
	    BroadBandTestGroup.REBOOT })
  @TestDetails(testUID = "TC-RDKB-REBOOT-1013")
  public void testToVerifyAbortReboot(Dut device) {
	// Variable to store testcaseID
	String testCaseId = "TC-RDKB-REBOOT-013";
	// Variable to store step number
	String stepNumber = "s1";
	// variable to store status
	boolean status = false;
	// Variable to store errorMessage
	String errorMessage = null;
	// Variable to store response
	String response = null;
	// Variable to store current image name
	String currentImageName = null;
	// Variable to store image name for CDL
	String imageNameForCdl = null;
	// Variable to store CDL logs for validation
	String cdlLogsForValidation = null;
	// Variable to store expected log message
	String expectedLogMessage = null;
	// Variable to store command
	String command = null;
	// Variable to store step counter
	int stepCounter = BroadBandTestConstants.CONSTANT_7;
	// Variable to store abort count
	int abortCount = BroadBandTestConstants.CONSTANT_1;
	// Variable to store start time
	long startTime = BroadBandTestConstants.CONSTANT_0;
	// Variable to store process id
	String processId = null;
	boolean hasLatestBuildChanged = false;
	boolean isXconfFileExist=false;
	boolean isCdlDataPosted = false;
	String currentImageNameforPostCondition = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv,
		device);
	try {
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STARTING TEST CASE: TC-RDKB-REBOOT-1013");
	    LOGGER.info("TEST DESCRIPTION: Verify for maximum abort reboot count");
	    LOGGER.info("TEST STEPS : ");
	    LOGGER.info("PRE-CONDITION 1 : Verify the current image name and image name for cdl");
	    LOGGER.info("PRE-CONDITION 2 : Reboot the device and wait for ip accusition");
	    LOGGER.info("PRE-CONDITION 3 : DISABLE CODEBIG BY USING WEBPA");
	    LOGGER.info("PRE-CONDITION 4 : Set default value of DEFER FIRMWARE DOWNLOAD REBOOT 290");
	    LOGGER.info("1. Verify default value for abort reboot is false");
	    LOGGER.info("2. Execute WebPa command to abort the reboot");
	    LOGGER.info("3. Verify invalid request is received as response for reboot abort");
	    LOGGER.info("4. Configure XCONF server with required configuration using REST API");
	    LOGGER.info("5. Reboot the device to initiate firmware upgrade");
	    LOGGER.info("6. Verify XCONF firmware download triggered successfully");
	    LOGGER.info("7. Verify firmware download is successful");
	    LOGGER.info("8. Set and verify abort reboot value as true through WebPa command");
	    LOGGER.info("9. Verify 290 sleep process running using ps");
	    LOGGER.info("10. Verify abort count value in XCONF log file");
	    LOGGER.info("11. Set and verify abort reboot value as true through WebPa command");
	    LOGGER.info("12. Verify 290 sleep process running using ps");
	    LOGGER.info("13. Verify abort count value in XCONF log file");
	    LOGGER.info("14. Set and verify abort reboot value as true through WebPa command");
	    LOGGER.info("15. Verify 290 sleep process running using ps");
	    LOGGER.info("16. Verify abort count value in XCONF log file");
	    LOGGER.info("17. Set and verify abort reboot value as true through WebPa command");
	    LOGGER.info("18. Verify 290 sleep process running using ps");
	    LOGGER.info("19. Verify abort count value in XCONF log file");
	    LOGGER.info("20. Set and verify abort reboot value as true through WebPa command");
	    LOGGER.info("21. Verify 290 sleep process running using ps");
	    LOGGER.info("22. Verify abort count value in XCONF log file");
	    LOGGER.info("23. Verify abort count value reached maximum value");
	    LOGGER.info("24. Verify device rebooting after reboot count reaches maximum limit");
	    LOGGER.info("POST-CONDITION 1 : Verify that device is upgraded with the previous firmware version");
	    LOGGER.info("#######################################################################################");

	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    /**
	     * PRE-CONDITION 1 : VERIFY THE CURRENT IMAGE NAME AND IMAGE NAME FOR CDL
	     */
	    currentImageName = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device);
	    /**
	     * PRE-CONDITION 2 : REBOOT THE DEVICE AND WAIT FOR IP ACCUSITION
	     */
	    BroadBandPreConditionUtils.preConditionToRebootAndWaitForIpAccusition(device, tapEnv,
		    BroadBandTestConstants.CONSTANT_2);
	    /**
	     * PRE-CONDITION 3 : DISABLE CODEBIG BY USING WEBPA
	     */
	    BroadBandPreConditionUtils.executePreConditionToDisableCodeBigFirst(device, tapEnv, BroadBandTestConstants.CONSTANT_3);
	   
	    
	    /**
	     * PRE-CONDITION 4 : Set default value of DEFER FIRMWARE DOWNLOAD REBOOT 290
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("PRE-CONDITION 4: DESCRIPTION : Set default value of DEFER FIRMWARE DOWNLOAD REBOOT 290");
	    LOGGER.info(
		    "PRE-CONDITION 4: ACTION : Execute WebPa command for Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.RPC.DeferFWDownloadReboot");
	    LOGGER.info(
		    "PRE-CONDITION 4: EXPECTED : The webPA command should execute successfully");
	    LOGGER.info("**********************************************************************************");
               errorMessage = "Unable to set the webPA command";
	         status= BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
			    BroadBandWebPaConstants.WEBPA_PARAM_DEFER_FIRMWARE_DOWNLOAD_REBOOT,
			    WebPaDataTypes.INTEGER.getValue(),
			    BroadBandTestConstants.STRING_CONSTANT_290);

          
          if (status) {
		LOGGER.info("PRE-CONDITION 4: ACTUAL: Successfully Set default value of DEFER FIRMWARE DOWNLOAD REBOOT 290"
			+ BroadBandWebPaConstants.WEBPA_PARAM_DEFER_FIRMWARE_DOWNLOAD_REBOOT);
	    } else {
		LOGGER.error("PRE-CONDITION 4: ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    
	    LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");
	    /**
	     * Step 1 : Verify default value for abort reboot is false
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Verify default value for abort reboot is false");
	    LOGGER.info(
		    "STEP 1: ACTION : Execute WebPa command for Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.RPC.AbortReboot");
	    LOGGER.info(
		    "STEP 1: EXPECTED : The webPA command should execute successfully and return default value as false");
	    LOGGER.info("**********************************************************************************");
	    response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_ABORT_REBOOT);
	    LOGGER.info(
		    "STEP 1: WebPA Response from Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.RPC.AbortReboot parameter is: "
			    + response);
	    errorMessage = "Null Response is retrieved for webpa parameter:"
		    + BroadBandWebPaConstants.WEBPA_PARAM_ABORT_REBOOT;
	    if (CommonMethods.isNotNull(response)) {
		status = response.equalsIgnoreCase(BroadBandTestConstants.FALSE);
		errorMessage = "Abort reboot is not false by default:"
			+ BroadBandWebPaConstants.WEBPA_PARAM_ABORT_REBOOT;
	    }
	    if (status) {
		LOGGER.info("STEP 1: ACTUAL: Successfully verified default value of abort reboot"
			+ BroadBandWebPaConstants.WEBPA_PARAM_ABORT_REBOOT);
	    } else {
		LOGGER.error("STEP 1: ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

	    /**
	     * Step 2 : Verify WebPa parameter set for Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.RPC.AbortReboot
	     * as true
	     */
	    stepNumber = "s2";
	    status = false;
	    errorMessage = null;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 2: DESCRIPTION : Verify WebPa parameter set for Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.RPC.AbortReboot as true");
	    LOGGER.info(
		    "STEP 2: ACTION :  Set WebPa command for Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.RPC.AbortReboot as true");
	    LOGGER.info(
		    "STEP 2: EXPECTED : The WebPa command should execute successfully and value should be false or operation should fail");
	    LOGGER.info("**********************************************************************************");
	    try {
		status = !BroadBandWiFiUtils.setWebPaParams(device, BroadBandWebPaConstants.WEBPA_PARAM_ABORT_REBOOT,
			BroadBandTestConstants.TRUE, BroadBandTestConstants.CONSTANT_3);
		LOGGER.info("STEP 2: Is Abort Reboot in device Enabled: " + status);
	    } catch (Exception exception) {
		status = true;
		LOGGER.info("STEP 2: Set operation failed for Abort reboot");
	    }
	    errorMessage = "Failed to Set value for WebPa parameter:" + BroadBandWebPaConstants.WEBPA_PARAM_ABORT_REBOOT
		    + "with Value:" + BroadBandTestConstants.TRUE;
	    if (status) {
		LOGGER.info("STEP 2: ACTUAL: Successfully verified WebPa set operation for Abort reboot");
	    } else {
		LOGGER.error("STEP 2: ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

	    /**
	     * Step 3 : Verify invalid request is received as response for reboot abort
	     */
	    stepNumber = "s3";
	    status = false;
	    errorMessage = null;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION : Verify invalid request is received as response for reboot abort");
	    LOGGER.info(
		    "STEP 3: ACTION :  grep -i \"invalid request for parameter.*AbortReboot\" /rdklogs/logs/PAMlog.txt.0");
	    LOGGER.info("STEP 3: EXPECTED : Abort reboot output should be present in log file");
	    LOGGER.info("**********************************************************************************");
	    response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTraceConstants.LOG_MESSAGE_ABORT_REBOOT, BroadBandTestConstants.COMMAND_NTP_LOG_FILE);
	    LOGGER.info("STEP 3: Response is: " + response);
	    status = CommonUtils.isNotEmptyOrNull(response);
	    errorMessage = "Failed to retrieve log message " + BroadBandTraceConstants.LOG_MESSAGE_ABORT_REBOOT
		    + "from : " + BroadBandTestConstants.COMMAND_NTP_LOG_FILE;
	    if (status) {
		LOGGER.info("STEP 3: ACTUAL: Successfully validated reboot-pending notification in "
			+ BroadBandTestConstants.COMMAND_NTP_LOG_FILE);
	    } else {
		LOGGER.error("STEP 3: ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

	    /**
	     * Step 4 : Configure XCONF server with required configuration using REST API
	     */
	    stepNumber = "s4";
	    status = false;
	    errorMessage = null;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 4: DESCRIPTION : Configure XCONF server with required configuration using REST API");
	    LOGGER.info(
		    "STEP 4: ACTION :  configure XCONF server with latest image, protocol as HTTP and reboot immediately as true");
	    LOGGER.info(
		    "STEP 4: EXPECTED : Should be able to configure the code download configuration using XCONF Rest API and swupdate.conf should updated with xconf url");
	    LOGGER.info("**********************************************************************************");
	    imageNameForCdl = tapEnv.getLatestBuildImageVersionForCdlTrigger(device, false);
	    LOGGER.info("LATEST FIRMWARE VERSION: " + imageNameForCdl);
	    if (CommonMethods.isNull(imageNameForCdl)) {
	    LOGGER.info(
	    " GA image obtained from deployed version service is null. Hence getting the image from property file ");
	    imageNameForCdl = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
	    BroadBandPropertyKeyConstants.PARTIAL_PROPERTY_KEY_FOR_GA_IMAGE);
	    LOGGER.info("Latest Firmware version from property file: " + imageNameForCdl);
	    }
	    errorMessage = "Unable to retrieve latest image name for RDKB devices";
	    LOGGER.info("STEP 4: Latest Image to CDL is: " + imageNameForCdl);

	    if (CommonMethods.isNotNull(imageNameForCdl)) {
		BroadBandXconfCdlUtils.configureRdkbDeviceForXconfCdl(tapEnv, device, imageNameForCdl, true,
			BroadBandTestConstants.FIRMWARE_DOWNLOAD_PROTOCOL_HTTP);
		status = true;
		isCdlDataPosted = status;
	    }
	    if (status) {
		LOGGER.info(
			"STEP 4: ACTUAL: Successfully configured XCONF server with required configuration using REST API");
	    } else {
		LOGGER.error("STEP 4: ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

	    /**
	     * Step 5 : Reboot the device to initiate firmware upgrade
	     */
	    stepNumber = "s5";
	    status = false;
	    errorMessage = null;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 5: DESCRIPTION : Reboot the device to initiate firmware upgrade");
	    LOGGER.info("STEP 5: ACTION :  execute the command /sbin/reboot");
	    LOGGER.info("STEP 5: EXPECTED : STB should reboot");
	    LOGGER.info("**********************************************************************************");
	    status = CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv);
	    errorMessage = "Unable to connect to RDKB device after reboot";
	    if (status) {
		LOGGER.info("STEP 5: ACTUAL: RDKB device is rebooted successfully to initiate firmware upgrade.");
	    } else {
		LOGGER.error("STEP 5: ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

	    /**
	     * Step 6 : Verify XCONF firmware download triggered successfully
	     */
	    stepNumber = "s6";
	    status = false;
	    errorMessage = null;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 6: DESCRIPTION : Verify XCONF firmware download triggered successfully");
	    LOGGER.info("STEP 6: ACTION : check log message under /rdklogs/logs/xconf.txt.0");
	    LOGGER.info("STEP 6: EXPECTED : Code download triggered message should be logged in Xconf log");
	    LOGGER.info("**********************************************************************************");
	    if ( DeviceModeHandler.isDSLDevice(device)) {
		LOGGER.info("No Waiting For DSL devices");
	    } else {
		LOGGER.info("Waiting for 5 minutes to make sure the /rdklogs/logs/xconf.txt.0 is created in the box");
		do {
		    startTime = System.currentTimeMillis();
		    isXconfFileExist = CommonUtils.isFileExists(device, tapEnv,
			    BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0);
		} while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS
			&& !isXconfFileExist && BroadBandCommonUtils.hasWaitForDuration(tapEnv,
				BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
	    }
	    cdlLogsForValidation = FirmwareDownloadUtils.getCdlLogsForValidation(tapEnv, device);
	    errorMessage = "Failed to retrieve cdl logs for Validation";
	    currentImageName = BroadBandCommonUtils
		    .removeDifferentSignedExtensionsInRequestedBuildName(currentImageName);
	    errorMessage = "Failed to remove signed extension from current build name";

	    imageNameForCdl = BroadBandCommonUtils.removeDifferentSignedExtensionsInRequestedBuildName(imageNameForCdl);
	    errorMessage = "Failed to remove signed extension from image name for cdl";

	    expectedLogMessage = BroadBandCdlConstants.XCONF_CODE_DOWNLOAD_MESSAGE
		    .replaceAll(BroadBandCdlConstants.CONFIGURATION_CURRENT_IMAGE_NAME, currentImageName)
		    .replaceAll(BroadBandCdlConstants.CONFIGURATION_REQUESTED_IMAGE_NAME, imageNameForCdl);
	    status = CommonMethods.patternMatcher(cdlLogsForValidation.toLowerCase(), expectedLogMessage.toLowerCase());

	    errorMessage = "Unable to find the XCONF HTTP CDL accepted message '" + expectedLogMessage + "' in "
		    + BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0;
	    if (status) {
		hasLatestBuildChanged = status;
		LOGGER.info("STEP 6: ACTUAL: Successfully validated the XCONF HTTP CDL accepted message in "
			+ BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0);
	    } else {
		LOGGER.error("STEP 6: ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

	    /**
	     * Step 7 : Verify firmware download is successful
	     */
	    stepNumber = "s7";
	    status = false;
	    errorMessage = null;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 7: DESCRIPTION : Verify firmware download is successful");
	    LOGGER.info(
		    "STEP 7: ACTION : check the log message \"XCONF SCRIPT : HTTP download Successful\" under /rdklogs/logs/xconf.txt.0");
	    LOGGER.info("STEP 7: EXPECTED : Xconf log should have HTTP download Successful log message");
	    LOGGER.info("**********************************************************************************");
	    expectedLogMessage = FirmwareDownloadUtils.getCdlDownloadSuccessLog(device);
	    status = CommonUtils.isGivenStringAvailableInCommandOutput(cdlLogsForValidation.toLowerCase(),
		    expectedLogMessage.toLowerCase());
	    errorMessage = "Unable to retrieve Code download completed message from " + expectedLogMessage + " in "
		    + BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0;
	    if (status) {
		LOGGER.info("STEP 7: ACTUAL: Successfully validated the XCONF HTTP CDL completed message in "
			+ BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0);
	    } else {
		LOGGER.error("STEP 7: ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

	    while (stepCounter < BroadBandTestConstants.CONSTANT_22) {
		/**
		 * Step : 8,11,14,17,20 Verify 290 sleep process running using ps
		 */
		stepCounter += BroadBandTestConstants.CONSTANT_1;
		stepNumber = "s" + Integer.toString(stepCounter);
		status = false;
		errorMessage = null;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + Integer.toString(stepCounter)
			+ ": DESCRIPTION : Verify 290 sleep process running using ps");
		LOGGER.info("STEP " + Integer.toString(stepCounter) + ": ACTION : execute command ps | grep sleep");
		LOGGER.info(
			"STEP " + Integer.toString(stepCounter) + ": EXPECTED : Should run the sleep process for 290");
		LOGGER.info("**********************************************************************************");
		errorMessage = "Failed to get the response for \"ps | grep sleep\"";
		startTime = System.currentTimeMillis();
		do {
		    // get the all the process id for sleep
		    response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_PS_GREP_SLEEP);
		    if (CommonMethods.isNotNull(response)) {
			errorMessage = "Failed to get the process id for sleep 290";
			// get the process id for sleep 290 using pattern finder
			processId = CommonMethods.patternFinder(response,
				BroadBandTestConstants.PATTERN_TO_GET_PID_SLEEP_290);
			status = CommonMethods.isNotNull(processId);
			// Removing the Kill process for sleep 290 since the process restart automatically after 290
			// seconds
		    }
		} while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS
			&& !status && BroadBandCommonUtils.hasWaitForDuration(tapEnv,
				BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS));
		if (status) {
		    LOGGER.info("STEP " + Integer.toString(stepCounter)
			    + " : ACTUAL: Successfully verified the process sleep 290");
		} else {
		    LOGGER.error("STEP " + Integer.toString(stepCounter) + " : ACTUAL: " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

		/**
		 * Step : 9,12,15,18,21 Set and verify abort reboot value as true through WebPa command
		 */
		stepCounter += BroadBandTestConstants.CONSTANT_1;
		stepNumber = "s" + Integer.toString(stepCounter);
		status = false;
		errorMessage = null;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + Integer.toString(stepCounter)
			+ ": DESCRIPTION : Set and verify abort reboot value as true through WebPa command");
		LOGGER.info("STEP " + Integer.toString(stepCounter)
			+ ": ACTION : Set the WebPa parameter Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.RPC.AbortReboot as true");
		LOGGER.info(
			"STEP " + Integer.toString(stepCounter) + ": EXPECTED : Abort reboot value should set as true");
		LOGGER.info("**********************************************************************************");
		// set and verify the abort reboot as true using WebPA command
		status = BroadBandWebPaUtils.verifyWebPaValueAfterDuration(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_ABORT_REBOOT, BroadBandTestConstants.CONSTANT_3,
			BroadBandTestConstants.TRUE, BroadBandTestConstants.TWO_SECOND_IN_MILLIS);
		LOGGER.info(
			"STEP: " + Integer.toString(stepCounter) + ": Is Abort Reboot in device Enabled: " + status);
		errorMessage = "Failed to Set value for WebPa parameter: "
			+ BroadBandWebPaConstants.WEBPA_PARAM_ABORT_REBOOT + " with Value: "
			+ BroadBandTestConstants.TRUE;
		if (status) {
		    LOGGER.info("STEP " + Integer.toString(stepCounter)
			    + " : ACTUAL: Successfully Enabled Abort reboot to device");
		} else {
		    LOGGER.error("STEP " + Integer.toString(stepCounter) + " : ACTUAL: " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

		/**
		 * Step : 10,13,16,19,22 Verify abort count value in XCONF log file
		 */
		stepCounter += BroadBandTestConstants.CONSTANT_1;
		stepNumber = "s" + Integer.toString(stepCounter);
		status = false;
		errorMessage = null;
		startTime = System.currentTimeMillis();
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + Integer.toString(stepCounter)
			+ ": DESCRIPTION : Verify abort count value in XCONF log file");
		LOGGER.info("STEP " + Integer.toString(stepCounter)
			+ ": ACTION : execute command grep -i \"Abort Count is\" /rdklogs/logs/xconf.txt.0 | tail -1");
		LOGGER.info("STEP " + Integer.toString(stepCounter)
			+ ": EXPECTED : Abort count log message should present in XCONF log file");
		LOGGER.info("**********************************************************************************");
		errorMessage = "Failed to log Abort count in Xconf log file";
		command = BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.GREP_COMMAND,
			BroadBandTraceConstants.LOG_MESSAGE_ABORT_COUNT,
			BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0, BroadBandTestConstants.SYMBOL_PIPE,
			BroadBandTestConstants.CMD_TAIL_1);
		do {
		    response = tapEnv.executeCommandUsingSsh(device, command);
		    if (CommonMethods.isNotNull(response)) {
			response = CommonMethods.patternFinder(response, "Abort\\s+Count\\s+is\\s+(\\d+)");
			LOGGER.info("RESPONSE : " + response);
			if (CommonMethods.isNotNull(response)) {
			    LOGGER.info("STEP " + Integer.toString(stepCounter)
				    + ": xconf log message for abort count: " + response);
			    // abort count value is initialized to 1
			    // compare the abort count value is increased in the
			    // log message
			    status = Integer.toString(abortCount).equalsIgnoreCase(response);
			}
		    }
		} while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS
			&& !status && BroadBandCommonUtils.hasWaitForDuration(tapEnv,
				BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS));
		abortCount += BroadBandTestConstants.CONSTANT_1;
		if (status) {
		    LOGGER.info("STEP " + Integer.toString(stepCounter) + " : ACTUAL: Abort count log message is : "
			    + response);
		} else {
		    LOGGER.error("STEP " + Integer.toString(stepCounter) + " : ACTUAL: " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
		// break the loop if abort count reached maximum value
		if (abortCount == BroadBandTestConstants.CONSTANT_5
			&& Integer.toString(abortCount).equalsIgnoreCase(response)) {
		    break;
		}
	    }

	    /**
	     * Step 23 : Verify abort count value reached maxmium value
	     */
	    stepCounter += BroadBandTestConstants.CONSTANT_1;
	    stepNumber = "s" + Integer.toString(stepCounter);
	    status = false;
	    errorMessage = null;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + Integer.toString(stepCounter)
		    + ": DESCRIPTION : Verify abort count value reached maxmium value");
	    LOGGER.info("STEP " + Integer.toString(stepCounter)
		    + ": ACTION : execute command grep -i \"Abort Count reached maximum limit 5\" /rdklogs/logs/xconf.txt.0");
	    LOGGER.info("STEP " + Integer.toString(stepCounter)
		    + ": EXPECTED : Abort count maximum limit log message should present in XCONF log file");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Failed to log message for abort count reached maximum limit in Xconf log file";
	    command = BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.GREP_COMMAND,
		    BroadBandTraceConstants.LOG_MESSAGE_MAXIMUM_ABORT_COUNT,
		    BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0);
	    response = tapEnv.executeCommandUsingSsh(device, command);
	    status = CommonMethods.isNotNull(response);
	    if (status) {
		LOGGER.info("STEP " + Integer.toString(stepCounter) + ": ACTUAL: Abort count reached maximum limit");
	    } else {
		LOGGER.error("STEP " + Integer.toString(stepCounter) + ": ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

	    /**
	     * Step 24 : Verify device rebooting after reboot count reaches maximum limit
	     */
	    stepCounter += BroadBandTestConstants.CONSTANT_1;
	    stepNumber = "s" + Integer.toString(stepCounter);
	    status = false;
	    errorMessage = null;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + Integer.toString(stepCounter)
		    + ": DESCRIPTION : Verify device rebooting after reboot count reaches maximum limit");
	    LOGGER.info("STEP " + Integer.toString(stepCounter) + ": ACTION : check device is accessible or not");
	    LOGGER.info("STEP " + Integer.toString(stepCounter)
		    + ": EXPECTED : Device should reboot after reboot count reaches maximum limit");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Device is accessible after reboot count reaches maximum limit";
	    status = BroadBandCommonUtils.verifySTBRebootAndStbAccessible(device, tapEnv);
	    if (status) {
		LOGGER.info("STEP " + Integer.toString(stepCounter)
			+ ": ACTUAL: device rebooted successfully after reaching maximum abort count vaue");
	    } else {
		LOGGER.error("STEP " + Integer.toString(stepCounter) + ": ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("Exception Occurred while abort count maximum limit validation:" + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
		    true);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    
	    status = false;
	    errorMessage = "Failed to SSH to Device";
	    status = CommonMethods.isSTBAccessible(device) ? CommonUtils.rebootAndWaitForIpAcquisition(tapEnv, device)
		    : CommonMethods.waitForEstbIpAcquisition(tapEnv, device);
	    if (status) {
		LOGGER.info("Reverting back to the default image:" + currentImageNameforPostCondition);
		FirmwareDownloadUtils.deleteSoftwareUpdateConfigurationFile(tapEnv, device);

		status = BroadBandCodeDownloadUtils.triggerPreviousCodeDownload(device, tapEnv,
			currentImageNameforPostCondition);
		errorMessage = "Failed to downgrade image to default image";
		FirmwareDownloadUtils.deleteSoftwareUpdateConfigurationFile(tapEnv, device);
	    }
	    
	    if (status) {
		LOGGER.info("POST-CONDITION : ACTUAL: Image Reverted Successfully");
	    } else {
		LOGGER.error("ACTUAL: " + errorMessage);
	    }
	   
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-REBOOT-1013");
	LOGGER.info("#######################################################################################");
  }
  
  /**
  *
  * Test Case # 2: Verify Abort Reboot after successful CDL
  *
  * <p>
  * STEPS:
  * </p>
  * <ol>
  * <li>STEP 1: Set and verify defer firmware download reboot value through webpa command other than default value
  * </li>
  * <li>STEP 2: Configure XCONF server with required configuration using REST API</li>
  * <li>STEP 3: Reboot the device and check whether device comes up properly</li>
  * <li>STEP 4: Verify XCONF firmware download triggered successfully</li>
  * <li>STEP 5: Verify firmware download is successful</li>
  * <li>STEP 6: Verify defer firmware download reboot value in log message</li>
  * <li>STEP 7: Set and verify abort reboot value through webpa command other than default value</li>
  * <li>STEP 8: Verify STB is accessible after enabling abort reboot</li>
  * </ol>
  *
  * @author Sumathi Gunasekaran
  * @Refactor Rakesh C N
  * @param device
  *            {@link Dut}
  */
 @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
 @TestDetails(testUID = "TC-RDKB-REBOOT-1014")
 public void testToVerifyAbortRebootAfterCDL(Dut device) {
	// Variable to store testcaseID
	String testCaseId = "TC-RDKB-REBOOT-014";
	// Variable to store step number
	String stepNumber = "s1";
	// variable to store status
	boolean status = false;
	// Variable to store errorMessage
	String errorMessage = null;
	// Variable to store response
	String response = null;
	// Variable to store current image name
	String currentImageName = null;
	// Variable to store imageNameForCdl
	String imageNameForCdl = null;
	// Variable to store expectedLogMessage
	String expectedLogMessage = null;
	// Variable to store cdlLogsForValidation
	String cdlLogsForValidation = null;
	// Variable to store image name for post condition
	String currentImageNameforPostCondition = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv,
		device);
	try {
	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    /**
	     * PRE-CONDITION 1 : Pre-Condition method to disable code big first.
	     */
	    BroadBandPreConditionUtils.executePreConditionToDisableCodeBigFirst(device, tapEnv,
		    BroadBandTestConstants.CONSTANT_1);
	    LOGGER.info("#######################################################################################");
	    
	    // STEP 1: Set and verify defer firmware download reboot value through webpa command other than default
	    // value
	    LOGGER.info("************************************************************");
	    LOGGER.info(
		    "STEP 1: Set and verify defer firmware download reboot value through webpa command other than default value");
	    LOGGER.info("STEP 1 "
		    + ": ACTION : Execute Command:Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.RPC.DeferFWDownloadReboot ");
	    LOGGER.info("EXPECTED: Webpa command should execute successfully and return value");
	    LOGGER.info("************************************************************");
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEFER_FIRMWARE_DOWNLOAD_REBOOT,
		    BroadBandTestConstants.CONSTANT_2, BroadBandTestConstants.STRING_CONSTANT_520);
	    LOGGER.info("Is defer firmware download reboot value is set other than default: " + status);
	    errorMessage = "Failed to Set value for WebPa parameter:"
		    + BroadBandWebPaConstants.WEBPA_PARAM_DEFER_FIRMWARE_DOWNLOAD_REBOOT + "with Value:"
		    + BroadBandTestConstants.STRING_CONSTANT_520;
	    LOGGER.info("STEP : ACTUAL: "
		    + (status ? "defer firmware download reboot value is set successfully" : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

	    // STEP 2: Configure XCONF server with required configuration using REST API
	    stepNumber = "s2";
	    status = false;
	    errorMessage = null;
	    LOGGER.info("************************************************************");
	    LOGGER.info("STEP 2: Configure XCONF server with required configuration using REST API");
	    LOGGER.info("STEP 2: ACTION : Get latest and current image name.");
	    LOGGER.info(
		    "EXPECTED: Should be able to configure the code download configuration using XCONF Rest API and swupdate.conf should updated with xconf url");
	    LOGGER.info("************************************************************");
	    currentImageName = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device);
	    imageNameForCdl = tapEnv.getLatestBuildImageVersionForCdlTrigger(device, false);
	    LOGGER.info("LATEST FIRMWARE VERSION: " + imageNameForCdl);
	    if (CommonMethods.isNull(imageNameForCdl)) {
	    LOGGER.info(
	    " GA image obtained from deployed version service is null. Hence getting the image from property file ");
	    imageNameForCdl = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
	    BroadBandPropertyKeyConstants.PARTIAL_PROPERTY_KEY_FOR_GA_IMAGE);
	    LOGGER.info("Latest Firmware version from property file: " + imageNameForCdl);
	    }
	    errorMessage = "Unable to retrieve latest image name for RDKB devices";
	    LOGGER.info("Latest Image to CDL is: " + imageNameForCdl);
	    if (CommonMethods.isNotNull(imageNameForCdl)) {
		BroadBandXconfCdlUtils.configureRdkbDeviceForXconfCdl(tapEnv, device, imageNameForCdl, true,
			XconfConstants.FIRMWARE_DOWNLOAD_PROTOCOL_HTTP);
		status = true;
	    }
	    LOGGER.info("STEP : ACTUAL: "
		    + (status ? "Successfully configured XCONF server with required configuration using REST API"
			    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
	    // STEP 3 Reboot the device and check whether device comes up properly.
	    stepNumber = "s3";
	    status = false;
	    errorMessage = null;
	    LOGGER.info("************************************************************");
	    LOGGER.info("STEP 3:  Reboot the device to initiate firmware upgrade.");
	    LOGGER.info("STEP 3 : ACTION : EXECUTE reboot COMMAND");
	    LOGGER.info("EXPECTED: STB should reboot.");
	    LOGGER.info("************************************************************");
	    status = CommonUtils.rebootAndWaitForIpAcquisition(tapEnv, device);
	    errorMessage = "Unable to connect to RDKB device after reboot";
	    LOGGER.info("STEP : ACTUAL: "
		    + (status ? "RDKB device is rebooted successfully to initiate firmware upgrade." : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

	    // STEP 4: Verify XCONF firmware download triggered successfully
	    stepNumber = "s4";
	    status = false;
	    errorMessage = null;
	    LOGGER.info("************************************************************");
	    LOGGER.info("STEP 4:  Verify XCONF firmware download triggered successfully");
	    LOGGER.info("STEP 4: ACTION : Verify XCONF SCRIPT RESPONSE in /rdklogs/logs/xconf.txt.0");
	    LOGGER.info("EXPECTED: Code download triggered message should be logged in Xconf log");
	    LOGGER.info("************************************************************");
	    cdlLogsForValidation = FirmwareDownloadUtils.getCdlLogsForValidation(tapEnv, device);
	    currentImageName = BroadBandCommonUtils
		    .removeDifferentSignedExtensionsInRequestedBuildName(currentImageName);
	    imageNameForCdl = BroadBandCommonUtils.removeDifferentSignedExtensionsInRequestedBuildName(imageNameForCdl);
	    expectedLogMessage = BroadBandCdlConstants.XCONF_CODE_DOWNLOAD_MESSAGE
		    .replaceAll(BroadBandCdlConstants.CONFIGURATION_CURRENT_IMAGE_NAME, currentImageName)
		    .replaceAll(BroadBandCdlConstants.CONFIGURATION_REQUESTED_IMAGE_NAME, imageNameForCdl);
	    status = CommonMethods.patternMatcher(cdlLogsForValidation.toLowerCase(), expectedLogMessage.toLowerCase());

	    errorMessage = "Unable to find the XCONF HTTP CDL accepted message '" + expectedLogMessage + "' in "
		    + BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0;
	    LOGGER.info("STEP :ACTUAL: " + (status ? "Successfully validated the XCONF HTTP CDL accepted message in "
		    + BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0 : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

	    // STEP 5: Verify firmware download is successful
	    stepNumber = "s5";
	    status = false;
	    errorMessage = null;
	    LOGGER.info("************************************************************");
	    LOGGER.info("STEP 5: Verify firmware download is successful");
	    LOGGER.info("STEP 5: ACTION : Verify XCONF HTTP CDL completed message in /rdklogs/logs/xconf.txt.0");
	    LOGGER.info("EXPECTED: Xconf log should have HTTP download Successful log message");
	    LOGGER.info("************************************************************");
	    expectedLogMessage = FirmwareDownloadUtils.getCdlDownloadSuccessLog(device);
	    status = CommonUtils.isGivenStringAvailableInCommandOutput(cdlLogsForValidation.toLowerCase(),
		    expectedLogMessage.toLowerCase());
	    errorMessage = "Unable to retrieve Code download completed message from " + expectedLogMessage + " in "
		    + BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0;

	    LOGGER.info("STEP : ACTUAL: " + (status ? "Successfully validated the XCONF HTTP CDL completed message in "
		    + BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0 : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

	    // STEP 6: Verify defer firmware download reboot value in PAM log message
	    stepNumber = "s6";
	    status = false;
	    errorMessage = null;
	    LOGGER.info("************************************************************");
	    LOGGER.info("STEP 6: Verify defer firmware download reboot value in log message");
	    LOGGER.info("STEP 6: ACTION : Verify reboot-pending notification in /rdklogs/logs/PAMlog.txt.0");
	    LOGGER.info("EXPECTED: reboot-pending notification should be present in PAM log file");
	    LOGGER.info("************************************************************");
	    response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTraceConstants.LOG_MESSAGE_REBOOT_PENDING_NOTIFICATION,
		    BroadBandTestConstants.COMMAND_NTP_LOG_FILE, BroadBandTestConstants.TEN_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    status = CommonMethods.isNotNull(response);
	    errorMessage = "Failed to retrieve log message:"
		    + BroadBandTraceConstants.LOG_MESSAGE_REBOOT_PENDING_NOTIFICATION + " from :"
		    + BroadBandTestConstants.COMMAND_NTP_LOG_FILE;
	    LOGGER.info("STEP : ACTUAL: " + (status ? "Successfully validated reboot-pending notification in "
		    + BroadBandTestConstants.COMMAND_NTP_LOG_FILE : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

	    // STEP 7: Set and verify abort reboot value through webpa command other than default value
	    stepNumber = "s7";
	    status = false;
	    errorMessage = null;
	    LOGGER.info("************************************************************");
	    LOGGER.info("STEP 7: Set and verify abort reboot value through webpa command other than default value");
	    LOGGER.info("STEP 7: ACTION : Execute Command: Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.RPC.AbortReboot");
	    LOGGER.info("EXPECTED: The webPA command should execute successfully and return value as true");
	    LOGGER.info("************************************************************");
	    status = BroadBandWiFiUtils.setWebPaParams(device, BroadBandWebPaConstants.WEBPA_PARAM_ABORT_REBOOT,
		    BroadBandTestConstants.TRUE, BroadBandTestConstants.CONSTANT_3);
	    LOGGER.info("Is Abort Reboot in device Enabled: " + status);
	    errorMessage = "Failed to Set value for WebPa parameter:" + BroadBandWebPaConstants.WEBPA_PARAM_ABORT_REBOOT
		    + "with Value:" + BroadBandTestConstants.TRUE;
	    LOGGER.info("ACTUAL: " + (status ? "Successfully Enabled Abort reboot to device with webpa parameter:"
		    + BroadBandWebPaConstants.WEBPA_PARAM_ABORT_REBOOT : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

	    // STEP 8: Wait for firmware download deferred time and verify device aborts reboot
	    stepNumber = "s8";
	    status = false;
	    errorMessage = null;
	    LOGGER.info("************************************************************");
	    LOGGER.info("STEP 8: Verify STB is accesible after enabling abort reboot");
	    LOGGER.info("STEP 8: ACTION : Wait for 10 min to check reboot has aborted check if stb is accessible");
	    LOGGER.info("EXPECTED: STB should be accessible.");
	    LOGGER.info("************************************************************");
	    LOGGER.info("Waiting for Deferred firmware download delay ");
	    tapEnv.waitTill(BroadBandTestConstants.TEN_MINUTE_IN_MILLIS);
	    status = CommonMethods.isSTBAccessible(device);
	    errorMessage = "STB is not Accessible even after aborting reboot";
	    LOGGER.info("ACTUAL: " + (status ? "Is STB Accesible after setting Abort Reboot:" : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("Exception Occurred while Aborting the reboot after successful CDL:" + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
		    true);
	} finally {
	    // POST CONDITION
	    // Reconfiguring the cdl server with previous image
	    status = false;
	    errorMessage = "Failed to SSH to Device";
	    status = CommonMethods.isSTBAccessible(device) ? CommonUtils.rebootAndWaitForIpAcquisition(tapEnv, device)
		    : CommonMethods.waitForEstbIpAcquisition(tapEnv, device);
	    if (status) {
		LOGGER.info("Reverting back to the default image:" + currentImageNameforPostCondition);
		FirmwareDownloadUtils.deleteSoftwareUpdateConfigurationFile(tapEnv, device);
		status = BroadBandCodeDownloadUtils.triggerPreviousCodeDownload(device, tapEnv,
			currentImageNameforPostCondition);
		errorMessage = "Failed to downgrade image to default image";
		FirmwareDownloadUtils.deleteSoftwareUpdateConfigurationFile(tapEnv, device);
	    }
	    LOGGER.info((status ? "POST-CONDITION : ACTUAL: Image Reverted Successfully" : errorMessage));
	}
 }
}