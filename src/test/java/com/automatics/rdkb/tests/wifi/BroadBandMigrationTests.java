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

package com.automatics.rdkb.tests.wifi;

import java.util.HashMap;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandCdlConstants;
import com.automatics.rdkb.constants.BroadBandPropertyKeyConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.TR69ParamConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandPostConditionUtils;
import com.automatics.rdkb.utils.BroadBandRestoreWifiUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.cdl.BroadBandXconfCdlUtils;
import com.automatics.rdkb.utils.cdl.FirmwareDownloadUtils;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpMib;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;
import com.automatics.utils.xconf.XConfUtils;
import com.automatics.webpa.WebPaServerResponse;

/**
 * Class for migration tests
 * 
 * @author anandam.s
 * @refactor yamini.s
 *
 */

public class BroadBandMigrationTests extends AutomaticsTestBase {

	/** SLF4j logger. */
	public static final Logger LOGGER = LoggerFactory.getLogger(BroadBandMigrationTests.class);

	/** Map to store the precondition values of wifi webpa parameters */
	Map<String, String> preConditionWifiWebpaParameterList = new HashMap<String, String>();

	/** Map to store the precondition values of wifi webpa parameters */
	Map<String, String> preConditionWifiSnmpParameterList = new HashMap<String, String>();

	/** Map to store the precondition values of wifi webpa parameters */
	Map<String, String> postConditionWifiWebpaParameterList = new HashMap<String, String>();

	/** Map to store the precondition values of wifi webpa parameters */
	Map<String, String> postConditionWifiSnmpParameterList = new HashMap<String, String>();

	/** String to store the initial image name in device */
	String imageNameBeforeCodeDownload = null;

	/** Boolean value to store the latest build changed */
	boolean hasLatestBuildChanged = false;

	/** Boolean value to store the original build changed */
	boolean hasOriginalBuildChanged = false;

	/** Constant holds the Current Firmware version **/
	private static String initialFirmwareVersion = null;

	/** Constant holds the cdl data posted to xconf **/
	boolean isCdlDataPosted = false;

	/**
	 * Test to Verify WiFi settings after Migration to QA Release.
	 * 
	 * <li>STEP 1:Using Deployed version service get the latest deployed GA
	 * build</li>
	 * <li>STEP 2:Configure /opt/swupdate.conf file with Mock Xconf url</li>
	 * <li>STEP 3:Configure XCONF firmware download details in Mock Server</li>
	 * <li>STEP 4: Trigger CDL to the latest deployed GA build.</li>
	 * <li>STEP 5:Verify CDL has started</li>
	 * <li>STEP 6:Verify CDL has Completed</li>
	 * <li>STEP 7: Reboot the device and wait till the devices acquires IP</li>
	 * <li>STEP 8: Verify the latest image version in the device</li>
	 * <li>STEP 9: Get Private 24ghz SSID name and confirm it is same as before
	 * device upgrade using WEBPA</li>
	 * <li>STEP 10: Get Private 5ghz SSID name and confirm it is same as before
	 * device upgrade using WEBPA</li>
	 * <li>TEP 11: Get Private 24ghz SSID password and confirm it is same as before
	 * device upgrade using WEBPA</li>
	 * <li>STEP 12: Get Private 5ghz SSID password and confirm it is same as before
	 * device upgrade using WEBPA</li>
	 * <li>STEP 13: Get Private 24ghz Operating Standard and confirm it is same as
	 * before device upgrade using WEBPA</li>
	 * <li>STEP 14: Get Private 5ghz Operating Standard and confirm it is same as
	 * before device upgrade using WEBPA</li>
	 * <li>STEP 15:Verify Wifi network channel bandwidth for 5GHz Wifi band</li>
	 * <li>STEP 16: Verify Wifi Broadcast network Name enabled status for 5 GHz Wifi
	 * band</li>
	 * <li>STEP 17: Verify Guard interval for 5 GHz Wifi network</li>
	 * <li>STEP 18: Verify WiFi network active status for 5GHz in Connection Status
	 * page</li>
	 * <li>STEP 19: Verify the WiFi supported protocols for 5 Ghz band</li>
	 * 
	 * @param device {@link Dut}
	 * @author anandam.s
	 * @refactor yamini.s
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = BroadBandTestGroup.WIFI)
	@TestDetails(testUID = "TC-RDKB-WIFI-MIGRATION-1001")
	public void testVerifyMigrationToQARelease(Dut device) {
		// Test case id
		String testId = "TC-RDKB-WIFI-MIGRATION-101";
		// Test step number
		String testStepNumber = "s1";
		// String to store the error message
		String errorMessage = null;
		int postConStepNumber = 1;
		hasLatestBuildChanged = false;
		hasOriginalBuildChanged = false;
		try {

			LOGGER.info("#######################################################################################");
			LOGGER.info("STARTING TEST CASE: " + testId);
			LOGGER.info("TEST DESCRIPTION: Verify WiFi settings after Migration to QA Release");
			LOGGER.info("*************************************************************************");

			LOGGER.debug(
					"************************************************************************************************");
			LOGGER.debug("Precondition:Set the below parametrs to a non  default value using webpa");
			LOGGER.debug("1. SSID name");
			LOGGER.debug("2. Network Security mode ");
			LOGGER.debug("3. Network Enabled status");
			LOGGER.debug("4. Channel Selection Mode ");
			LOGGER.debug("5. Channel bandwidth");
			LOGGER.debug("6. Netwok name enabled sttaus ");
			LOGGER.debug("7. Guard interval");
			LOGGER.debug("8. Network active status");
			LOGGER.debug("9.Supported Protocols");
			LOGGER.debug("EXPECTED: All the test parameters must be set to non default values");
			LOGGER.debug("STEP 1:Using Deployed version service get the latest deployed GA build ");
			LOGGER.debug("EXPECTED: Reponse will have details of latest deployed version ");
			LOGGER.debug("STEP 2:Configure /opt/swupdate.conf file with Mock Xconf url");
			LOGGER.debug("EXPECTED: Xconf url should be configured successfully'");
			LOGGER.debug("STEP 3:Configure XCONF firmware download details in Mock Server ");
			LOGGER.debug("EXPECTED: Configured successfully");
			LOGGER.debug("STEP 4: Trigger CDL to the latest deployed GA build. ");
			LOGGER.debug("EXPECTED: Webpa should be set as true and CDL should be triggered");
			LOGGER.debug("STEP 5:Verify CDL has started ");
			LOGGER.debug("EXPECTED:Webpa should return \"In Progress\"");
			LOGGER.debug("STEP 6:Verify CDL has Completed ");
			LOGGER.debug("EXPECTED:Webpa should return \"Completed\"");
			LOGGER.debug("STEP 7: Reboot the device  and wait till the devices acquires IP ");
			LOGGER.debug("EXPECTED: device should acquire ip after reboot within 6 minutes ");
			LOGGER.debug("STEP 8: Verify the latest image version in the device ");
			LOGGER.debug("EXPECTED: The command output should contain the QA image for which Xconf CDL was triggered");
			LOGGER.debug(
					"STEP 9: Get Private 24ghz SSID name and confirm it is same as before device upgrade using WEBPA");
			LOGGER.debug("EXPECTED: Expected o/p should be \"testing-2.4G SSID\" for example");
			LOGGER.debug(
					"STEP 10: Get  Private 5ghz SSID name and confirm it is same as before device upgrade using WEBPA");
			LOGGER.debug("EXPECTED: Expected o/p should be \"testing-5G SSID\" for example");
			LOGGER.debug(
					"STEP 11: Get  Private 24ghz SSID password and confirm it is same as before device upgrade using WEBPA");
			LOGGER.debug("EXPECTED:Expected o/p should be <pwd> for example");
			LOGGER.debug(
					"STEP 12: Get  Private 5ghz SSID password and confirm it is same as before device upgrade using WEBPA");
			LOGGER.debug("EXPECTED:Expected o/p should be <pwd> for example ");
			LOGGER.debug(
					"STEP 13: Get  Private 24ghz Operating Standard and confirm it is same as before device upgrade using WEBPA");
			LOGGER.debug("EXPECTED:Expected o/p should be \"g,n\" for example");
			LOGGER.debug(
					"STEP 14: Get  Private 5ghz Operating Standard and confirm it is same as before device upgrade using WEBPA");
			LOGGER.debug("EXPECTED:Expected o/p should be \"a,n,ac\" for example");
			LOGGER.debug("STEP 15:Verify Wifi network channel bandwidth for 5GHz Wifi band");
			LOGGER.debug("EXPECTED:The value should be 80 Mhz");
			LOGGER.debug("STEP 16: Verify Wifi Broadcast network Name enabled status for 5 GHz Wifi band");
			LOGGER.debug("EXPECTED:The value should be 'true'");
			LOGGER.debug("STEP 17: Verify Guard interval for 5 GHz Wifi network");
			LOGGER.debug("EXPECTED:The Guard interval should be 'Auto'");
			LOGGER.debug("STEP 18: Verify WiFi network active status for 5GHz in Connection Status page");
			LOGGER.debug("EXPECTED:The value should be 'true'");
			LOGGER.debug("STEP 19: Verify the WiFi supported protocols for 5 Ghz band");
			LOGGER.debug("EXPECTED:The supported protocols should be 'a,n,ac'");
			LOGGER.debug(
					"************************************************************************************************");

			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION : DESCRIPTION : Get the the below Parameters using  webpa");
			LOGGER.info("1. 2.4Ghz SSID name");
			LOGGER.info("2. 5Ghz SSID name");
			LOGGER.info("3. 2.4Ghz SSID password");
			LOGGER.info("4. 5Ghz SSID password");
			LOGGER.info("5. 24ghz Operating Standard");
			LOGGER.info("6. 5ghz Operating Standard ");
			LOGGER.info("7. 24ghz Radio Status");
			LOGGER.info("8. 5ghz Radio Status");
			LOGGER.info("9. 24ghz SSID Status");
			LOGGER.info("10. 5ghz SSID Status");
			LOGGER.info("Get the the below Parameters using  webpa");
			LOGGER.info("1. 2.4Ghz SSID name");
			LOGGER.info("2. 5Ghz SSID name");
			LOGGER.info("3. 2.4Ghz SSID password");
			LOGGER.info("4. 5Ghz SSID password");
			LOGGER.info("5. 24ghz SSID Status");
			LOGGER.info("6. 5ghz SSID Status");
			LOGGER.info("PRE-CONDITION : ACTION  : Execute webpa and snmp commands for the parameters ");
			LOGGER.info("PRE-CONDITION : EXPECTED: A non null value should be obtained");

			boolean status = getWifiParametersUsingWebpaAndSnmpBeforeOrAfterCDL(device, tapEnv, true);
			if (!status) {
				LOGGER.error("All SNMP or WEBPA parameters are not retrieved successfully in precondition");
			}
			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

			LOGGER.info("******************************************************");
			LOGGER.info("STEP 1: DESCRIPTION : Using Deployed version service get the latest deployed GA build ");
			LOGGER.info("STEP 1: ACTION      : Execute http://<deployed-version-service-url>/<model>/gAcceptance ");
			LOGGER.info("STEP 1: EXPECTED    : Reponse will have details of latest deployed version");
			LOGGER.info("******************************************************");
			testStepNumber = "s1";
			errorMessage = "Failed to get the latest deployed version";
			String buildNameToBeTriggerred = null;
			LOGGER.info("Getting Firmware Version to be downlaoed");
			try {
				buildNameToBeTriggerred = tapEnv.getLatestBuildImageVersionForCdlTrigger(device, false);
				if (CommonMethods.isNull(buildNameToBeTriggerred)) {
					LOGGER.info(
							" GA image obtained from deployed version service is null. Hence getting the image from property file ");
					buildNameToBeTriggerred = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(
							device, BroadBandPropertyKeyConstants.PARTIAL_PROPERTY_KEY_FOR_GA_IMAGE);
					LOGGER.info("Latest Firmware version from property file: " + buildNameToBeTriggerred);
				}
				LOGGER.info("image obtained:  " + buildNameToBeTriggerred);
			} catch (TestException e) {
				LOGGER.error(e.getMessage());
			}
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, null != buildNameToBeTriggerred, errorMessage,
					true);

			boolean isBothImageSame = BroadBandRestoreWifiUtils.verifyWhetherImageIsAlreadyFlashedInDevice(device,
					tapEnv, buildNameToBeTriggerred);
			if (!isBothImageSame) {
				/** s2 to s8 */
				initialFirmwareVersion = buildNameToBeTriggerred;
				executeStepsForCDL(device, tapEnv, testId, new String[] { "s2", "s3", "s4", "s5", "s6", "s7", "s8" },
						buildNameToBeTriggerred, true);
				hasLatestBuildChanged = true;
				/** s9 to s24 */
				compareParameterValuesAfterCDL(device, tapEnv, testId, new String[] { "s9", "s10", "s11", "s12", "s13",
						"s14", "s15", "s16", "s17", "s18", "s19", "s20", "s21", "s22", "s23", "s24", "s25", "s26" });

				/** s25 to s31 */
				// do CDL to initial Image
				executeStepsForCDL(device, tapEnv, testId,
						new String[] { "s27", "s28", "s29", "s30", "s31", "s32", "s33", }, imageNameBeforeCodeDownload,
						true);
				hasOriginalBuildChanged = true;

				/** s32 to s47 */
				compareParameterValuesAfterCDL(device, tapEnv, testId, new String[] { "s34", "s35", "s36", "s37", "s38",
						"s39", "s40", "s41", "s42", "s43", "s44", "s45", "s46", "s47", "s48", "s49", "s50", "s51" });
			} else {
				LOGGER.error(
						"Current Image in box and Build name obtained from deployed version service are same .So test cannot be continued ");
				LOGGER.error("Marking all steps as NA");
				for (int i = 2; i <= 52; i++) {
					tapEnv.updateExecutionForAllStatus(device, testId, "s".concat(String.valueOf(i)),
							ExecutionStatus.NOT_APPLICABLE,
							"Current Image in box and Build name obtained from deployed version service are same .So test cannot be continued ",
							false);
				}
			}

		} catch (Exception exception) {
			LOGGER.error("Exception occured during execution !!!!" + exception.getMessage());
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, false, errorMessage, false);
		} finally {
			if (isCdlDataPosted || hasLatestBuildChanged) {
				LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
				/**
				 * POST CONDITION 1 : CLEAR THE CDL INFORMATION URL IN /NVRAM/SWUPDATE.CONF
				 */
				if (isCdlDataPosted) {
					BroadBandPostConditionUtils.executePostConditionToClearCdlInfoInXconf(device, tapEnv,
							postConStepNumber);
				}
				/**
				 * POST CONDITION 2 : TRIGGER ORIGINAL BUILD ON THE DEVICE
				 */
				BroadBandPostConditionUtils.executePostConditionToTriggerCdl(device, tapEnv, hasLatestBuildChanged,
						hasOriginalBuildChanged, postConStepNumber++, imageNameBeforeCodeDownload);
				LOGGER.info("################### ENDING POST-CONFIGURATIONS ###################");
				LOGGER.debug("ENDING TESTCASE : TC-RDKB-WIFI-MIGRATION-1001");
			}
		}
	}

	/**
	 * This method executes the precondition for testVerifyMigrationToQARelease .If
	 * isPrecondition is set as true( ie before CDL ),the method will execute both
	 * webpa and snmp parameters and add it a map.If isPrecondition is set as false(
	 * ie after CDL ) it will execute and add to a different map.These two maps can
	 * later be used for comparison
	 * 
	 * @param device         {@link Dut}
	 * @param tapEnv         {@link AutomaticsTapApi}
	 * @param isPrecondition true if we are executing precondition steps
	 * 
	 * @refactor yamini.s
	 */
	private boolean getWifiParametersUsingWebpaAndSnmpBeforeOrAfterCDL(Dut device, AutomaticsTapApi tapEnv,
			boolean isPrecondition) {
		boolean statusforWebpa = false;
		boolean statusforSnmp = false;
		String errorMessage = "Error : ";
		String errorMessgeStringAppender = isPrecondition ? "precondition" : "postcondition";
		try {

			String[] paramList = { BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2GHZ_SECURITY_KEYPASSPHRASE,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5GHZ_SECURITY_KEYPASSPHRASE,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_2_4_GHZ_OPERATING_STANDARD,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_5GHZ_OPERATING_STANDARD,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFI_2_4_RADIO_ENABLE,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFI_5_RADIO_ENABLE,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLE,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED_STATUS,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4_GHZ_PRIVATE_SECURITY_MODEENABLED,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5_GHZ_PRIVATE_SECURITY_MODEENABLED };

			String[] oidNames = { BroadBandSnmpMib.ECM_WIFI_SSID_2_4.getOid(),
					BroadBandSnmpMib.ECM_WIFI_SSID_5.getOid(), BroadBandSnmpMib.ECM_WIFI_2_4_PASSPHRASE.getOid(),
					BroadBandSnmpMib.ECM_WIFI_5_PASSPHRASE.getOid(), BroadBandSnmpMib.ECM_WIFI_2_4_SSID_STATUS.getOid(),
					BroadBandSnmpMib.ECM_WIFI_5_SSID_STATUS.getOid()

			};
			if (isPrecondition) {

				preConditionWifiWebpaParameterList = tapEnv.executeMultipleWebPaGetCommands(device, paramList);
				if (null != preConditionWifiWebpaParameterList && !preConditionWifiWebpaParameterList.isEmpty()
						&& preConditionWifiWebpaParameterList.size() == paramList.length) {
					statusforWebpa = true;
				}

				for (String parameter : oidNames) {
					String snmpOutput = BroadBandSnmpUtils.snmpWalkOnEcm(tapEnv, device, parameter);
					if (CommonMethods.isNotNull(snmpOutput)
							&& !snmpOutput.contains(BroadBandTestConstants.NO_SUCH_INSTANCE)
							&& !snmpOutput.contains(BroadBandTestConstants.NO_SUCH_OBJECT_AVAILABLE)) {
						preConditionWifiSnmpParameterList.put(parameter, snmpOutput);
					} else {
						LOGGER.error("Error in Getting the value of OID :" + parameter);
						errorMessage += parameter;
					}
				}
				if (preConditionWifiSnmpParameterList.size() == oidNames.length) {
					statusforSnmp = true;
				} else {
					LOGGER.error("Error in following OIDs : " + errorMessage);
				}

				imageNameBeforeCodeDownload = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device);
				LOGGER.info("Image name before CDL  : " + imageNameBeforeCodeDownload);

			} else {
				postConditionWifiWebpaParameterList = tapEnv.executeMultipleWebPaGetCommands(device, paramList);
				if (null != postConditionWifiWebpaParameterList && !postConditionWifiWebpaParameterList.isEmpty()
						&& postConditionWifiWebpaParameterList.size() == paramList.length) {
					statusforWebpa = true;
				}
				for (String parameter : oidNames) {
					String snmpOutput = BroadBandSnmpUtils.snmpWalkOnEcm(tapEnv, device, parameter);
					if (CommonMethods.isNotNull(snmpOutput)
							&& !snmpOutput.contains(BroadBandTestConstants.NO_SUCH_INSTANCE)
							&& !snmpOutput.contains(BroadBandTestConstants.NO_SUCH_OBJECT_AVAILABLE)) {
						postConditionWifiSnmpParameterList.put(parameter, snmpOutput);
					} else {
						LOGGER.error("Error in Getting the value of OID :" + parameter);
						errorMessage += parameter;
					}
				}
				if (postConditionWifiSnmpParameterList.size() == oidNames.length) {
					statusforSnmp = true;
				} else {
					LOGGER.error("Error in following OIDs : " + errorMessage);
				}
			}
		} catch (Exception e) {
			LOGGER.error("Exception while executing the " + errorMessgeStringAppender
					+ " .All SNMP and Webpa WIFI parameters may not have been pouplated" + e.getMessage());
		}
		return statusforWebpa && statusforSnmp;
	}

	/**
	 * This method execute the CDL steps and verify new image was loaded
	 * 
	 * @param device                  {@link Dut}
	 * @param tapEnv                  {@link AutomaticsTapApi}
	 * @param testId                  Test ID under execution
	 * 
	 * @param testStepNumbers         step numbers executed in this function
	 * @param buildNameToBeTriggerred image name for CDL
	 * 
	 * @refactor yamini.s
	 */
	private void executeStepsForCDL(Dut device, AutomaticsTapApi tapEnv, String testId, String[] testStepNumbers,
			String buildNameToBeTriggerred, boolean isStepUpdateRquired) {
		String testStepNumber = "DEFAULT";
		boolean status = false;
		String errorMessage = null;
		if (null != testStepNumbers && isStepUpdateRquired) {
			testStepNumber = testStepNumbers[0];
		}
		status = false;
		errorMessage = "Mock server configuration failed ";
		if (isStepUpdateRquired) {
			LOGGER.info("******************************************************");
			LOGGER.info(
					"STEP" + testStepNumber + ": DESCRIPTION : Configure /opt/swupdate.conf file with Mock Xconf url ");
			LOGGER.info("STEP " + testStepNumber
					+ ": ACTION      : execute echo \"https://<XCONF_URL>/xconf/swu/stb\" > /nvram/swupdate.conf");
			LOGGER.info("STEP " + testStepNumber + ": EXPECTED    : Xconf url should be configured successfully");
			LOGGER.info("******************************************************");
		}

		try {
			// Configure /opt/swupdate.conf file with Mock Xconf url
			BroadBandXconfCdlUtils.updateSoftwareUpdateConfigurationOnClient(tapEnv, device);
			status = true;
			isCdlDataPosted = status;
			LOGGER.info("Successfully configured /opt/swupdate.conf with Mock Xconf url");
		} catch (Exception e) {
			status = false;
			errorMessage = "Exception while configuring mock server." + e.getMessage();
			LOGGER.error(errorMessage);
		}
		if (isStepUpdateRquired) {
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
		}

		if (null != testStepNumbers && isStepUpdateRquired) {
			testStepNumber = testStepNumbers[1];
		}
		status = false;
		errorMessage = "Failed to Configure XCONF firmware download details in Mock Server";
		if (isStepUpdateRquired) {
			LOGGER.info("******************************************************");
			LOGGER.info("STEP " + testStepNumber
					+ ": DESCRIPTION :Configure XCONF firmware download details in Mock Server ");
			LOGGER.info(
					"STEP " + testStepNumber + ": ACTION: set reboot immediately- true, protocol -http ,build name");
			LOGGER.info("STEP " + testStepNumber + ": EXPECTED: Configured successfully");
			LOGGER.info("******************************************************");
		}
		try {
			XConfUtils.configureXconfDownloadFirmwareDetails(device, buildNameToBeTriggerred, false,
					BroadBandTestConstants.FIRMWARE_DOWNLOAD_PROTOCOL_HTTP);
			status = true;
			LOGGER.info("SUCCESSFULLY CONFIGURED XCONF FIRMWARE DOWNLOAD DETAILS IN MOCK SERVER");
		} catch (Exception e) {
			status = false;
			errorMessage = "Exception while configuring firmware download details in mock server." + e.getMessage();
			LOGGER.error(errorMessage);
		}
		if (isStepUpdateRquired) {
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
		}
		tapEnv.waitTill(BroadBandTestConstants.FIVE_SECONDS_IN_MILLIS);

		if (null != testStepNumbers && isStepUpdateRquired) {
			testStepNumber = testStepNumbers[2];
		}
		status = false;
		errorMessage = "Failed to Trigger CDL to the latest deployed GA build";
		if (isStepUpdateRquired) {
			LOGGER.info("******************************************************");
			LOGGER.info("STEP " + testStepNumber + ": DESCRIPTION :Trigger CDL to the latest deployed GA build ");
			LOGGER.info("STEP " + testStepNumber
					+ ": ACTION: set webpa Device.X_COMCAST-COM_Xcalibur.Client.xconfCheckNow as true");
			LOGGER.info(
					"STEP " + testStepNumber + ": EXPECTED: Webpa should be set as true and CDL should be triggered");
			LOGGER.info("******************************************************");

		}

		// Set Xconf Check Now parameter to trigger the CDL
		WebPaServerResponse response = BroadBandWebPaUtils.setWebPaParamAndReturnResp(tapEnv, device,
				TR69ParamConstants.TR69_XCONF_CHECK_NOW, "true", BroadBandTestConstants.CONSTANT_3);
		status = (response.getStatusCode() == BroadBandTestConstants.SUCCESS_CODE)
				|| response.getMessage().contains(BroadBandTestConstants.SUCCESS_TXT);
		LOGGER.info("Download triggered Status: " + status);
		if (isStepUpdateRquired) {
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
		}

		if (null != testStepNumbers && isStepUpdateRquired) {
			testStepNumber = testStepNumbers[3];
		}
		long startTime = System.currentTimeMillis();
		errorMessage = "Failed to verify that CDL has started";
		if (isStepUpdateRquired) {
			LOGGER.info("******************************************************");
			LOGGER.info("STEP " + testStepNumber + ": DESCRIPTION :Verify CDL has started  ");
			LOGGER.info("STEP " + testStepNumber
					+ ": ACTION: get webpa Device.DeviceInfo.X_RDKCENTRAL-COM_FirmwareDownloadStatus");
			LOGGER.info("STEP " + testStepNumber + ": EXPECTED: Webpa should return \"In Progress\"");
			LOGGER.info("******************************************************");
		}
		status = false;
		do {
			String downloadStatus = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_Device_DeviceInfo_X_RDKCENTRAL_COM_FirmwareDownloadStatus);
			if (downloadStatus.contains(BroadBandCdlConstants.CDL_STATUS_IN_PROGRESS)) {
				status = true;
				break;
			} else if (downloadStatus.contains(BroadBandCdlConstants.CDL_STATUS_COMPLETED)) {
				LOGGER.error("In Progress state has not been returned. But CDL has been completed");
				status = false;
				break;
			}
		} while (!status && (System.currentTimeMillis() - startTime) < BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS
				&& BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.TWO_SECOND_IN_MILLIS));
		LOGGER.info("Step " + testStepNumber + ": ACTUAL : Download Inprogress status: " + status);
		if (isStepUpdateRquired) {
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
		}

		if (null != testStepNumbers && isStepUpdateRquired) {
			testStepNumber = testStepNumbers[4];
		}
		status = false;
		if (isStepUpdateRquired) {
			LOGGER.info("***********************************************************************************");
			LOGGER.info("STEP " + testStepNumber + ": DESCRIPTION :Verify CDL has completed  ");
			LOGGER.info("STEP " + testStepNumber
					+ ": ACTION: get webpa Device.DeviceInfo.X_RDKCENTRAL-COM_FirmwareDownloadStatus");
			LOGGER.info("STEP " + testStepNumber + ": EXPECTED: Webpa should return \"Completed\"");
			LOGGER.info("******************************************************");
		}
		startTime = System.currentTimeMillis();
		errorMessage = "Failed to verify that CDL has not completed";
		String downloadStatus = null;
		do {
			downloadStatus = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_Device_DeviceInfo_X_RDKCENTRAL_COM_FirmwareDownloadStatus);
			status = downloadStatus.contains(BroadBandCdlConstants.CDL_STATUS_COMPLETED);
		} while (!status && (System.currentTimeMillis() - startTime) < BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS
				&& BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.TEN_SECOND_IN_MILLIS));
		LOGGER.info("Step " + testStepNumber + ": ACTUAL : Download Completed status: " + status
				+ "Download status returned from webpa : " + downloadStatus);
		if (isStepUpdateRquired) {
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
		}
		status = false;
		if (null != testStepNumbers && isStepUpdateRquired) {
			testStepNumber = testStepNumbers[5];
		}
		if (isStepUpdateRquired) {
			LOGGER.info("***********************************************************************************");
			LOGGER.info("STEP " + testStepNumber
					+ ": DESCRIPTION :Reboot the device  and wait till the devices acquires IP   ");
			LOGGER.info("STEP " + testStepNumber
					+ ": ACTION: Execute /sbin/reboot .Wait till for Maximum 6 minutes for the device to become up after reboot");
			LOGGER.info(
					"STEP " + testStepNumber + ": EXPECTED: device should acquire ip after reboot within 6 minutes ");
			LOGGER.info("******************************************************");
		}
		try {
			status = CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv);
			if (!status) {
				errorMessage = "Failed to reboot and get IP after reboot";
				LOGGER.error(errorMessage);
			}
		} catch (Exception e) {
			errorMessage = "Exception during rebooting and waiting for IP";
			LOGGER.error(errorMessage);
		}

		LOGGER.info("Step " + testStepNumber + ": ACTUAL " + status);
		if (isStepUpdateRquired) {
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
		}

		if (null != testStepNumbers && isStepUpdateRquired) {
			testStepNumber = testStepNumbers[6];
		}
		status = false;
		errorMessage = "Both Image names are different";
		if (isStepUpdateRquired) {
			LOGGER.info("***********************************************************************************");
			LOGGER.info("STEP " + testStepNumber + ": DESCRIPTION :Verify the latest image version in the device    ");
			LOGGER.info("STEP " + testStepNumber + ": ACTION: Execute head /version.txt in device  \"");
			LOGGER.info("STEP " + testStepNumber
					+ ": EXPECTED: The command output should contain the QA image for which Xconf CDL was triggered ");
			LOGGER.info("***********************************************************************************");
		}
		tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);
		// Verify the image version using TR69 command FirmwareFilename
		LOGGER.info("Checking whether the image in the device is " + buildNameToBeTriggerred);
		status = BroadBandRestoreWifiUtils.verifyWhetherImageIsAlreadyFlashedInDevice(device, tapEnv,
				buildNameToBeTriggerred);
		if (!status) {
			errorMessage = "Current firmware version  and CDL triggered image are differnt .SO CDL failed!!!!! +EXPECTED : "
					+ buildNameToBeTriggerred;
			LOGGER.error(errorMessage);
		}
		if (isStepUpdateRquired) {
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
		}
	}

	/**
	 * Compare pre and post SNMP and Webpa values for all Wifi parameters
	 * 
	 * @param device
	 * @param tapEnv
	 * @param testId
	 * @param testStepNumbers
	 * 
	 * @refactor yamini.s
	 */
	private void compareParameterValuesAfterCDL(Dut device, AutomaticsTapApi tapEnv, String testId,
			String[] testStepNumbers) {
		boolean status = false;
		String testStepNumber = testStepNumbers[0];
		String errorMessage = null;

		// get all webpa and snmp parameters post CDL
		status = getWifiParametersUsingWebpaAndSnmpBeforeOrAfterCDL(device, tapEnv, false);
		if (!status) {
			LOGGER.error("All SNMP or WEBPA parameters are not retrieved successfully");
		}

		status = false;
		testStepNumber = testStepNumbers[0];
		errorMessage = "Both parameter values are different";
		LOGGER.info("***********************************************************************************");
		LOGGER.info("STEP " + testStepNumber
				+ ": DESCRIPTION :Get Private 24ghz SSID name and confirm it is same as before device upgrade using WEBPA ");
		LOGGER.info("STEP " + testStepNumber + ": ACTION: Device.WiFi.SSID.10001.SSID\"");
		LOGGER.info("STEP " + testStepNumber + ": EXPECTED:  o/p should be same as before CDL ");
		LOGGER.info("***********************************************************************************");
		try {
			status = BroadBandRestoreWifiUtils.compareWifiSettingsWithWebpaValuesBeforeAndAfterCDL(
					preConditionWifiWebpaParameterList, postConditionWifiWebpaParameterList,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_NAME);

		} catch (TestException e) {
			LOGGER.error(errorMessage);
		}
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

		status = false;
		testStepNumber = testStepNumbers[1];
		errorMessage = "Both parameter values are different";
		LOGGER.info("***********************************************************************************");
		LOGGER.info("STEP " + testStepNumber
				+ ": DESCRIPTION :Get Private 5ghz SSID name and confirm it is same as before device upgrade using WEBPA ");
		LOGGER.info("STEP " + testStepNumber + ": ACTION: Device.WiFi.SSID.10101.SSID\"");
		LOGGER.info("STEP " + testStepNumber + ": EXPECTED:  o/p should be same as before CDL ");
		LOGGER.info("***********************************************************************************");
		try {
			status = BroadBandRestoreWifiUtils.compareWifiSettingsWithWebpaValuesBeforeAndAfterCDL(
					preConditionWifiWebpaParameterList, postConditionWifiWebpaParameterList,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_NAME);

		} catch (TestException e) {
			LOGGER.error(errorMessage);
		}
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

		status = false;
		testStepNumber = testStepNumbers[2];
		errorMessage = "Both parameter values are different";
		LOGGER.info("***********************************************************************************");
		LOGGER.info("STEP " + testStepNumber
				+ ": DESCRIPTION :Get Private 24ghz SSID password and confirm it is same as before device upgrade using WEBPA ");
		LOGGER.info("STEP " + testStepNumber + ": ACTION: Device.WiFi.AccessPoint.10001.Security.KeyPassphrase\"");
		LOGGER.info("STEP " + testStepNumber + ": EXPECTED:  o/p should be same as before CDL ");
		LOGGER.info("***********************************************************************************");
		try {
			status = BroadBandRestoreWifiUtils.compareWifiSettingsWithWebpaValuesBeforeAndAfterCDL(
					preConditionWifiWebpaParameterList, postConditionWifiWebpaParameterList,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2GHZ_SECURITY_KEYPASSPHRASE);

		} catch (TestException e) {
			LOGGER.error(errorMessage);
		}
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

		status = false;
		testStepNumber = testStepNumbers[3];
		errorMessage = "Both parameter values are different";
		LOGGER.info("***********************************************************************************");
		LOGGER.info("STEP " + testStepNumber
				+ ": DESCRIPTION :Get Private 5ghz SSID password and confirm it is same as before device upgrade using WEBPA ");
		LOGGER.info("STEP " + testStepNumber + ": ACTION: Device.WiFi.AccessPoint.10101.Security.KeyPassphrase\"");
		LOGGER.info("STEP " + testStepNumber + ": EXPECTED:  o/p should be same as before CDL ");
		LOGGER.info("***********************************************************************************");
		try {
			status = BroadBandRestoreWifiUtils.compareWifiSettingsWithWebpaValuesBeforeAndAfterCDL(
					preConditionWifiWebpaParameterList, postConditionWifiWebpaParameterList,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5GHZ_SECURITY_KEYPASSPHRASE);

		} catch (TestException e) {
			LOGGER.error(errorMessage);
		}
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

		status = false;
		testStepNumber = testStepNumbers[4];
		errorMessage = "Both parameter values are different";
		LOGGER.info("***********************************************************************************");
		LOGGER.info("STEP " + testStepNumber
				+ ": DESCRIPTION :Get Private 24ghz Operating Standard  and confirm it is same as before device upgrade using WEBPA ");
		LOGGER.info("STEP " + testStepNumber + ": ACTION: Device.WiFi.Radio.10000.OperatingStandards\"");
		LOGGER.info("STEP " + testStepNumber + ": EXPECTED:  o/p should be same as before CDL ");
		LOGGER.info("***********************************************************************************");
		try {
			status = BroadBandRestoreWifiUtils.compareWifiSettingsWithWebpaValuesBeforeAndAfterCDL(
					preConditionWifiWebpaParameterList, postConditionWifiWebpaParameterList,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_2_4_GHZ_OPERATING_STANDARD);

		} catch (TestException e) {
			LOGGER.error(errorMessage);
		}
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

		status = false;
		testStepNumber = testStepNumbers[5];
		errorMessage = "Both parameter values are different";
		LOGGER.info("***********************************************************************************");
		LOGGER.info("STEP " + testStepNumber
				+ ": DESCRIPTION :Get Private 5ghz Operating Standard and confirm it is same as before device upgrade using WEBPA ");
		LOGGER.info("STEP " + testStepNumber + ": ACTION: Device.WiFi.Radio.10000.OperatingStandards\"");
		LOGGER.info("STEP " + testStepNumber + ": EXPECTED:  o/p should be same as before CDL ");
		LOGGER.info("***********************************************************************************");
		try {
			status = BroadBandRestoreWifiUtils.compareWifiSettingsWithWebpaValuesBeforeAndAfterCDL(
					preConditionWifiWebpaParameterList, postConditionWifiWebpaParameterList,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_5GHZ_OPERATING_STANDARD);

		} catch (TestException e) {
			LOGGER.error(errorMessage);
		}
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

		status = false;
		testStepNumber = testStepNumbers[6];
		errorMessage = "Both parameter values are different";
		LOGGER.info("***********************************************************************************");
		LOGGER.info("STEP " + testStepNumber
				+ ": DESCRIPTION :Get Private 24ghz radio status  and confirm it is same as before device upgrade using WEBPA ");
		LOGGER.info("STEP " + testStepNumber + ": ACTION: Device.WiFi.Radio.10000.Enable\"");
		LOGGER.info("STEP " + testStepNumber + ": EXPECTED:  o/p should be same as before CDL ");
		LOGGER.info("***********************************************************************************");
		try {
			status = BroadBandRestoreWifiUtils.compareWifiSettingsWithWebpaValuesBeforeAndAfterCDL(
					preConditionWifiWebpaParameterList, postConditionWifiWebpaParameterList,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFI_2_4_RADIO_ENABLE);

		} catch (TestException e) {
			LOGGER.error(errorMessage);
		}
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

		status = false;
		testStepNumber = testStepNumbers[7];
		errorMessage = "Both parameter values are different";
		LOGGER.info("***********************************************************************************");
		LOGGER.info("STEP " + testStepNumber
				+ ": DESCRIPTION :Get Private 5ghz Radio status and confirm it is same as before device upgrade using WEBPA ");
		LOGGER.info("STEP " + testStepNumber + ": ACTION: Device.WiFi.Radio.10000.Enable\"");
		LOGGER.info("STEP " + testStepNumber + ": EXPECTED:  o/p should be same as before CDL ");
		LOGGER.info("***********************************************************************************");
		try {
			status = BroadBandRestoreWifiUtils.compareWifiSettingsWithWebpaValuesBeforeAndAfterCDL(
					preConditionWifiWebpaParameterList, postConditionWifiWebpaParameterList,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFI_5_RADIO_ENABLE);

		} catch (TestException e) {
			LOGGER.error(errorMessage);
		}
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

		status = false;
		testStepNumber = testStepNumbers[8];
		errorMessage = "Both parameter values are different";
		LOGGER.info("***********************************************************************************");
		LOGGER.info("STEP " + testStepNumber
				+ ": DESCRIPTION :Get Private 24ghz security mode and confirm it is same as before device upgrade using WEBPA ");
		LOGGER.info("STEP " + testStepNumber + ": ACTION: Device.WiFi.AccessPoint.10001.Security.ModeEnabled\"");
		LOGGER.info("STEP " + testStepNumber + ": EXPECTED:  o/p should be same as before CDL ");
		LOGGER.info("***********************************************************************************");
		try {
			status = BroadBandRestoreWifiUtils.compareWifiSettingsWithWebpaValuesBeforeAndAfterCDL(
					preConditionWifiWebpaParameterList, postConditionWifiWebpaParameterList,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4_GHZ_PRIVATE_SECURITY_MODEENABLED);

		} catch (TestException e) {
			LOGGER.error(errorMessage);
		}
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

		status = false;
		testStepNumber = testStepNumbers[9];
		errorMessage = "Both parameter values are different";
		LOGGER.info("***********************************************************************************");
		LOGGER.info("STEP " + testStepNumber
				+ ": DESCRIPTION :Get Private 5ghz security mode and confirm it is same as before device upgrade using WEBPA ");
		LOGGER.info("STEP " + testStepNumber + ": ACTION: Device.WiFi.AccessPoint.10101.Security.ModeEnabled\"");
		LOGGER.info("STEP " + testStepNumber + ": EXPECTED:  o/p should be same as before CDL ");
		LOGGER.info("***********************************************************************************");
		try {
			status = BroadBandRestoreWifiUtils.compareWifiSettingsWithWebpaValuesBeforeAndAfterCDL(
					preConditionWifiWebpaParameterList, postConditionWifiWebpaParameterList,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5_GHZ_PRIVATE_SECURITY_MODEENABLED);

		} catch (TestException e) {
			LOGGER.error(errorMessage);
		}
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

		status = false;
		testStepNumber = testStepNumbers[10];
		errorMessage = "Both parameter values are different";
		LOGGER.info("***********************************************************************************");
		LOGGER.info("STEP " + testStepNumber
				+ ": DESCRIPTION :Get Private 24ghz SSID status  and confirm it is same as before device upgrade using WEBPA ");
		LOGGER.info("STEP " + testStepNumber + ": ACTION: Device.WiFi.SSID.10001.Enable\"");
		LOGGER.info("STEP " + testStepNumber + ": EXPECTED:  o/p should be same as before CDL ");
		LOGGER.info("***********************************************************************************");
		try {
			status = BroadBandRestoreWifiUtils.compareWifiSettingsWithWebpaValuesBeforeAndAfterCDL(
					preConditionWifiWebpaParameterList, postConditionWifiWebpaParameterList,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLED_STATUS);

		} catch (TestException e) {
			LOGGER.error(errorMessage);
		}
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

		status = false;
		testStepNumber = testStepNumbers[11];
		errorMessage = "Both parameter values are different";
		LOGGER.info("***********************************************************************************");
		LOGGER.info("STEP " + testStepNumber
				+ ": DESCRIPTION :Get Private 5ghz SSID status and confirm it is same as before device upgrade using WEBPA ");
		LOGGER.info("STEP " + testStepNumber + ": ACTION: Device.WiFi.SSID.10101.Enable\"");
		LOGGER.info("STEP " + testStepNumber + ": EXPECTED:  o/p should be same as before CDL ");
		LOGGER.info("***********************************************************************************");
		try {
			status = BroadBandRestoreWifiUtils.compareWifiSettingsWithWebpaValuesBeforeAndAfterCDL(
					preConditionWifiWebpaParameterList, postConditionWifiWebpaParameterList,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED_STATUS);

		} catch (TestException e) {
			LOGGER.error(errorMessage);
		}
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

		status = false;
		testStepNumber = testStepNumbers[12];
		errorMessage = "Both parameter values are different";
		LOGGER.info("***********************************************************************************");
		LOGGER.info("STEP " + testStepNumber
				+ ": DESCRIPTION :Get Private 24ghz SSID name and confirm it is same as before device upgrade using SNMP");
		LOGGER.info("STEP " + testStepNumber + ": ACTION: Execute OID .1.3.6.1.4.1.17270.50.2.2.2.1.1.3.10001\"");
		LOGGER.info("STEP " + testStepNumber + ": EXPECTED:  o/p should be same as before CDL ");
		LOGGER.info("***********************************************************************************");
		try {
			status = BroadBandRestoreWifiUtils.compareWifiSettingsWithSNMPParameterValuesBeforeAndAfterCDL(
					preConditionWifiSnmpParameterList, postConditionWifiSnmpParameterList,
					BroadBandSnmpMib.ECM_WIFI_SSID_2_4.getOid());

		} catch (TestException e) {
			LOGGER.error(errorMessage);
		}
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

		status = false;
		testStepNumber = testStepNumbers[13];
		errorMessage = "Both parameter values are different";
		LOGGER.info("***********************************************************************************");
		LOGGER.info("STEP " + testStepNumber
				+ ": DESCRIPTION :Get Private 5ghz SSID name and confirm it is same as before device upgrade using SNMP");
		LOGGER.info("STEP " + testStepNumber + ": ACTION: Execute OID .1.3.6.1.4.1.17270.50.2.2.2.1.1.3.10101\"");
		LOGGER.info("STEP " + testStepNumber + ": EXPECTED:  o/p should be same as before CDL ");
		LOGGER.info("***********************************************************************************");
		try {
			status = BroadBandRestoreWifiUtils.compareWifiSettingsWithSNMPParameterValuesBeforeAndAfterCDL(
					preConditionWifiSnmpParameterList, postConditionWifiSnmpParameterList,
					BroadBandSnmpMib.ECM_WIFI_SSID_5.getOid());

		} catch (TestException e) {
			LOGGER.error(errorMessage);
		}
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

		status = false;
		testStepNumber = testStepNumbers[14];
		errorMessage = "Both parameter values are different";
		LOGGER.info("***********************************************************************************");
		LOGGER.info("STEP " + testStepNumber
				+ ": DESCRIPTION :Get Private 24ghz SSID password and confirm it is same as before device upgrade using SNMP");
		LOGGER.info("STEP " + testStepNumber + ": ACTION: Execute OID .1.3.6.1.4.1.17270.50.2.2.3.1.1.2.10001\"");
		LOGGER.info("STEP " + testStepNumber + ": EXPECTED:  o/p should be same as before CDL ");
		LOGGER.info("***********************************************************************************");
		try {
			status = BroadBandRestoreWifiUtils.compareWifiSettingsWithSNMPParameterValuesBeforeAndAfterCDL(
					preConditionWifiSnmpParameterList, postConditionWifiSnmpParameterList,
					BroadBandSnmpMib.ECM_WIFI_2_4_PASSPHRASE.getOid());

		} catch (TestException e) {
			LOGGER.error(errorMessage);
		}
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

		status = false;
		testStepNumber = testStepNumbers[15];
		errorMessage = "Both parameter values are different";
		LOGGER.info("***********************************************************************************");
		LOGGER.info("STEP " + testStepNumber
				+ ": DESCRIPTION :Get Private 5ghz SSID password and confirm it is same as before device upgrade using SNMP");
		LOGGER.info("STEP " + testStepNumber + ": ACTION: Execute OID .1.3.6.1.4.1.17270.50.2.2.3.1.1.2.10101\"");
		LOGGER.info("STEP " + testStepNumber + ": EXPECTED:  o/p should be same as before CDL ");
		LOGGER.info("***********************************************************************************");
		try {
			status = BroadBandRestoreWifiUtils.compareWifiSettingsWithSNMPParameterValuesBeforeAndAfterCDL(
					preConditionWifiSnmpParameterList, postConditionWifiSnmpParameterList,
					BroadBandSnmpMib.ECM_WIFI_5_PASSPHRASE.getOid());

		} catch (TestException e) {
			LOGGER.error(errorMessage);
		}
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

		status = false;
		testStepNumber = testStepNumbers[16];
		errorMessage = "Both parameter values are different";
		LOGGER.info("***********************************************************************************");
		LOGGER.info("STEP " + testStepNumber
				+ ": DESCRIPTION :Get Private 24ghz SSID status and confirm it is same as before device upgrade using SNMP");
		LOGGER.info("STEP " + testStepNumber + ": ACTION: Execute OID .1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10001\"");
		LOGGER.info("STEP " + testStepNumber + ": EXPECTED:  o/p should be same as before CDL ");
		LOGGER.info("***********************************************************************************");
		try {
			status = BroadBandRestoreWifiUtils.compareWifiSettingsWithSNMPParameterValuesBeforeAndAfterCDL(
					preConditionWifiSnmpParameterList, postConditionWifiSnmpParameterList,
					BroadBandSnmpMib.ECM_WIFI_2_4_SSID_STATUS.getOid());

		} catch (TestException e) {
			LOGGER.error(errorMessage);
		}
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

		status = false;
		testStepNumber = testStepNumbers[17];
		errorMessage = "Both parameter values are different";
		LOGGER.info("***********************************************************************************");
		LOGGER.info("STEP " + testStepNumber
				+ ": DESCRIPTION :Get Private 5ghz SSID status and confirm it is same as before device upgrade using SNMP");
		LOGGER.info("STEP " + testStepNumber + ": ACTION: Execute OID .1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10101\"");
		LOGGER.info("STEP " + testStepNumber + ": EXPECTED:  o/p should be same as before CDL ");
		LOGGER.info("***********************************************************************************");
		try {
			status = BroadBandRestoreWifiUtils.compareWifiSettingsWithSNMPParameterValuesBeforeAndAfterCDL(
					preConditionWifiSnmpParameterList, postConditionWifiSnmpParameterList,
					BroadBandSnmpMib.ECM_WIFI_5_SSID_STATUS.getOid());

		} catch (TestException e) {
			LOGGER.error(errorMessage);
		}
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
	}

}
